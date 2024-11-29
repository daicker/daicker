{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TupleSections #-}

module Language.Daicker.Bundler where

import Control.Comonad.Cofree (Cofree ((:<)))
import Control.Monad (join)
import Control.Monad.Error.Class (MonadError (throwError), liftEither)
import Control.Monad.Except (ExceptT, withExceptT)
import Control.Monad.IO.Class (MonadIO (..))
import Data.List (find)
import Data.Maybe (isJust)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Language.Daicker.AST
  ( Argument,
    Argument' (KeywordArgument, PositionedArgument),
    Export' (Export),
    Expr,
    Identifier,
    Identifier' (Identifier),
    Import,
    Import' (Import),
    ImportScope,
    ImportScope' (FullScope, PartialScope),
    Module,
    Module' (Module),
    Parameter,
    Parameter' (KeywordParameter, PositionedParameter),
    Statement,
    Statement' (SExpr, SType),
    Type,
    URL,
    URL' (LocalFile),
  )
import Language.Daicker.Error (RuntimeError (RuntimeError), StaticError (StaticError))
import Language.Daicker.Parser (pModule, parse)
import Language.Daicker.Span (Span)
import qualified Language.Daicker.StdLib as SL
import Language.Haskell.TH.Lens (HasName (name))
import System.Directory (doesFileExist)
import System.Exit (ExitCode (ExitFailure))
import System.IO (readFile)

data Bundle a = Bundle
  { moduleBundle :: ModuleBundle a,
    argumentBundle :: ArgumentBundle a
  }
  deriving (Show, Eq)

data ModuleBundle a = ModuleBundle
  { preludeModule :: Module a,
    currentModule :: Module a,
    dependencyModules :: [(URL a, ModuleBundle a)]
  }
  deriving (Show, Eq)

data ArgumentBundle a = ArgumentBundle
  { currentArguments :: [PackedArgument a],
    parentArguments :: Maybe (ArgumentBundle a)
  }
  deriving (Show, Eq)

type PackedArgument a = (String, (Expr a, Maybe (Type a)))

appendArgument :: ArgumentBundle a -> [PackedArgument a] -> ArgumentBundle a
appendArgument bundle args = ArgumentBundle args (Just bundle)

findExpr :: Bundle a -> String -> Maybe (Bundle a, Expr a)
findExpr
  bundle@( Bundle
             mb@( ModuleBundle
                    (_ :< Module _ _ preludeStatements)
                    (_ :< Module _ _ currentStatements)
                    dependencyModules
                  )
             argumentBundle
           )
  name =
    join
      $ find
        isJust
      $ [maybeArg, maybeExprFromCurrentModule]
        <> maybeExprFromDependencyModules
        <> [maybeExprFromPrelude]
    where
      -- find expr from arguments
      maybeArg = (Bundle mb emptyArgumentBundle,) <$> findExprFromArguments argumentBundle name
      -- find expr from current module
      maybeExprFromCurrentModule = (Bundle mb emptyArgumentBundle,) <$> findExprFromStatements currentStatements name
      -- find expr from dependency modules
      -- TODO: support namespace reference
      maybeExprFromDependencyModules =
        map
          ( ( \(mb, _ :< Module _ _ statements) ->
                (Bundle mb emptyArgumentBundle,) <$> findExprFromStatements statements name
            )
              . (\(_, mb) -> (mb, currentModule mb))
          )
          dependencyModules

      -- find expr from prelude
      maybeExprFromPrelude = (Bundle mb emptyArgumentBundle,) <$> findExprFromStatements preludeStatements name

findExprFromStatements :: [Statement a] -> String -> Maybe (Expr a)
findExprFromStatements statements name =
  case find (\(_ :< SExpr (_ :< Identifier name') _) -> name' == name) statements of
    Just (_ :< SExpr _ e) -> Just e
    Nothing -> Nothing

findExprFromArguments :: ArgumentBundle a -> String -> Maybe (Expr a)
findExprFromArguments bundle name =
  case find (\(n, _) -> n == name) (currentArguments bundle) of
    Just (_, (e, _)) -> Just e
    Nothing -> case parentArguments bundle of
      Just p -> findExprFromArguments p name
      Nothing -> Nothing

findType :: Bundle a -> String -> Maybe (Bundle a, Type a)
findType
  bundle@( Bundle
             mb@( ModuleBundle
                    (_ :< Module _ _ preludeStatements)
                    (_ :< Module _ _ currentStatements)
                    dependencyModules
                  )
             argumentBundle
           )
  name =
    join
      $ find
        isJust
      $ [maybeArg, maybeTypeFromCurrentModule]
        <> maybeTypeFromDependencyModules
        <> [maybeTypeFromPrelude]
    where
      -- find expr from arguments
      maybeArg = (bundle,) <$> findTypeFromArguments argumentBundle name
      -- find expr from current module
      maybeTypeFromCurrentModule = (Bundle mb emptyArgumentBundle,) <$> findTypeFromStatements currentStatements name
      -- find expr from dependency modules
      -- TODO: support namespace reference
      maybeTypeFromDependencyModules =
        map
          ( ( \(mb, _ :< Module _ _ statements) ->
                (Bundle mb emptyArgumentBundle,) <$> findTypeFromStatements statements name
            )
              . (\(_, mb) -> (mb, currentModule mb))
          )
          dependencyModules

      -- find expr from prelude
      maybeTypeFromPrelude = (Bundle mb emptyArgumentBundle,) <$> findTypeFromStatements preludeStatements name

findTypeFromArguments :: ArgumentBundle a -> String -> Maybe (Type a)
findTypeFromArguments bundle name =
  case find (\(n, (_, t)) -> n == name) (currentArguments bundle) of
    Just (_, (_, t)) -> t
    Nothing -> case parentArguments bundle of
      Just p -> findTypeFromArguments p name
      Nothing -> Nothing

findTypeFromStatements :: [Statement a] -> String -> Maybe (Type a)
findTypeFromStatements bundle name =
  case find (\(_ :< SType (_ :< Identifier name') _) -> name' == name) bundle of
    Just (_ :< SType _ t) -> Just t
    Nothing -> Nothing

emptyArgumentBundle :: ArgumentBundle a
emptyArgumentBundle = ArgumentBundle [] Nothing

isExpr :: Statement a -> Bool
isExpr (_ :< SExpr {}) = True
isExpr _ = False

isType :: Statement a -> Bool
isType (_ :< SType {}) = True
isType _ = False

loadModuleBundle :: Module Span -> ExceptT [StaticError Span] IO (ModuleBundle Span)
loadModuleBundle m@(_ :< Module is _ _) = do
  ms <-
    mapM
      ( \(_ :< Import _ _ url) -> do
          m <- readModule url
          b <- loadModuleBundle m
          pure (url, b)
      )
      is
  pure (ModuleBundle SL.prelude m ms)

readModule :: URL Span -> ExceptT [StaticError Span] IO (Module Span)
readModule (s :< LocalFile fileName) = do
  case lookup fileName SL.stdlib of
    Just m -> pure m
    Nothing -> do
      src <- withExceptT (\e -> [StaticError e s]) $ safeReadFile fileName
      (m, _) <- liftEither $ parse pModule fileName src
      pure m

safeReadFile :: FilePath -> ExceptT String IO Text
safeReadFile filePath = do
  existsFile <- liftIO $ doesFileExist filePath
  if existsFile
    then liftIO $ T.readFile filePath
    else throwError ("does not exist: " <> filePath)

packArg :: [Parameter ann] -> [Argument ann] -> ExceptT (RuntimeError ann) IO [PackedArgument ann]
packArg params args =
  (<>)
    <$> liftEither (zipPositionedArg positionedParams positionedArgs)
    <*> liftEither (zipKeywordArg keywordParams keywordArgs)
  where
    positionedParams = filter isPositionedParam params
    keywordParams = filter isKeywordParam params
    positionedArgs = filter isPositionedArg args
    keywordArgs = filter isKeywordArg args
    isPositionedParam :: Parameter ann -> Bool
    isPositionedParam (_ :< PositionedParameter {}) = True
    isPositionedParam _ = False
    isKeywordParam :: Parameter ann -> Bool
    isKeywordParam (_ :< KeywordParameter {}) = True
    isKeywordParam _ = False
    isPositionedArg :: Argument ann -> Bool
    isPositionedArg (_ :< PositionedArgument {}) = True
    isPositionedArg _ = False
    isKeywordArg :: Argument ann -> Bool
    isKeywordArg (_ :< KeywordArgument {}) = True
    isKeywordArg _ = False
    zipPositionedArg :: [Parameter ann] -> [Argument ann] -> Either (RuntimeError ann) [PackedArgument ann]
    zipPositionedArg (p@(_ :< PositionedParameter _ _ _ t _) : ps) ((_ :< PositionedArgument False a) : as) = ((paramName p, (a, t)) :) <$> zipPositionedArg ps as
    zipPositionedArg [] _ = pure []
    zipKeywordArg :: [Parameter ann] -> [Argument ann] -> Either (RuntimeError ann) [PackedArgument ann]
    zipKeywordArg (p@(_ :< KeywordParameter (s :< Identifier name) _ _ t defaultValue) : ps) as = do
      value <- case find (\(_ :< KeywordArgument (_ :< Identifier name') e) -> name == name') as of
        Just (_ :< KeywordArgument _ a) -> pure a
        Nothing -> case defaultValue of
          Just e -> pure e
          Nothing -> Left $ RuntimeError ("Missing keyword argument: " <> name) s (ExitFailure 1)
      ((paramName p, (value, t)) :) <$> zipKeywordArg ps as
    zipKeywordArg [] _ = pure []
    paramName :: Parameter ann -> String
    paramName (_ :< PositionedParameter (i :< Identifier name) _ _ _ _) = name
    paramName (_ :< KeywordParameter (i :< Identifier name) _ _ _ _) = name

abandonType :: PackedArgument a -> (String, Expr a)
abandonType (n, (e, _)) = (n, e)
