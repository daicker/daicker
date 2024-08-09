module Language.Daicker.Bundler where

import Control.Comonad.Cofree (Cofree ((:<)))
import Control.Monad (join)
import Control.Monad.Error.Class (MonadError (throwError), liftEither)
import Control.Monad.Except (ExceptT, withExceptT)
import Control.Monad.IO.Class (MonadIO (..))
import Data.List (find)
import Data.Maybe (fromJust)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Language.Daicker.AST
  ( Define,
    Define'
      ( Define
      ),
    Export' (Export),
    Expr,
    Expr' (ENamespace, EObject),
    Identifier,
    Identifier' (Identifier),
    Import,
    Import' (NamedImport, PartialImport, WildImport),
    Module,
    Module' (Module),
    Statement,
    Statement' (SDefine, STypeDefine),
    TypeDefine' (TypeDefine),
    URL,
    URL' (LocalFile),
  )
import Language.Daicker.Error (RuntimeError, StaticError (StaticError))
import Language.Daicker.Lexer (lexTokens, mkTStreamWithoutComment)
import Language.Daicker.Parser (parseModule)
import Language.Daicker.Span (Span)
import Language.Daicker.StdLib (prelude)
import System.Directory (doesFileExist)
import System.IO (readFile)

findDefine :: String -> [Statement a] -> Maybe (Statement a)
findDefine n = find (\s -> isExpr s && name s == n)
  where
    name (_ :< SDefine (_ :< Define (_ :< Identifier n) _ _)) = n

findType :: String -> [Statement a] -> Maybe (Statement a)
findType n = find (\s -> isType s && name s == n)
  where
    name (_ :< STypeDefine (_ :< TypeDefine (_ :< Identifier n) _)) = n

data Bundle a = Bundle
  { modules :: ModuleBundle a,
    exprs :: ExprBundle a,
    current :: Module a
  }
  deriving (Show, Eq)

findExpr :: Bundle Span -> Identifier Span -> Either [StaticError] (Expr Span, Bundle Span)
findExpr (Bundle ms es c) (s :< Identifier name) = case lookup name es of
  Just (e, m) -> do
    es' <- loadExprs ms m
    pure (e, Bundle ms es' m)
  Nothing -> Left [StaticError ("not defined: " <> name) s]

type ExprBundle a = [(String, (Expr a, Module a))]

type ModuleBundle a = [(String, Module a)]

loadExprs :: ModuleBundle Span -> Module Span -> Either [StaticError] (ExprBundle Span)
loadExprs ms m@(s :< Module is e ss) = do
  let es = map (toPairExpr m) (pickupExpr ss)
  bs <- mapM (importedExprs ms) is
  ps <- exportedExprs prelude
  pure $ ps <> join bs <> es

importedExprs :: ModuleBundle Span -> Import Span -> Either [StaticError] (ExprBundle Span)
importedExprs ms (s :< NamedImport (_ :< Identifier name) (_ :< LocalFile url)) = do
  let m = fromJust $ lookup url ms
  exprs <- exportedExprs m
  pure [(name, (s :< ENamespace (map (\(k, (e, _)) -> (k, e)) exprs), m))]
importedExprs ms (s :< PartialImport _ (_ :< LocalFile url)) = Left [StaticError "partial import is not implemented yet" s]
importedExprs ms (s :< WildImport (_ :< LocalFile url)) = Left [StaticError "wild import is not implemented yet" s]

exportedExprs :: Module Span -> Either [StaticError] (ExprBundle Span)
exportedExprs m@(s :< Module _ (Just (_ :< Export names)) ss) = Left [StaticError "partial export is not implemented yet" s]
exportedExprs m@(_ :< Module _ Nothing ss) = pure $ map (toPairExpr m) (pickupExpr ss)

toPairExpr :: Module a -> Define a -> (String, (Expr a, Module a))
toPairExpr m (_ :< Define (_ :< Identifier name) e _) = (name, (e, m))

pickupExpr :: [Statement a] -> [Define a]
pickupExpr ss = map (\(_ :< SDefine d) -> d) $ filter isExpr ss

isExpr :: Statement a -> Bool
isExpr (_ :< SDefine {}) = True
isExpr _ = False

isType :: Statement a -> Bool
isType (_ :< STypeDefine {}) = True
isType _ = False

loadModules :: [Import Span] -> ExceptT [StaticError] IO (ModuleBundle Span)
loadModules = foldr (\i -> (<*>) ((<>) <$> importModules i)) (pure [])

importModules :: Import Span -> ExceptT [StaticError] IO (ModuleBundle Span)
importModules (s :< NamedImport _ url) = readModule url
importModules (s :< PartialImport _ url) = readModule url
importModules (s :< WildImport url) = readModule url

readModule :: URL Span -> ExceptT [StaticError] IO (ModuleBundle Span)
readModule (s :< LocalFile fileName) = do
  src <- withExceptT (\e -> [StaticError e s]) $ safeReadFile fileName
  tokens <- liftEither $ lexTokens fileName src
  let stream = mkTStreamWithoutComment src tokens
  m@(_ :< Module is _ _) <- liftEither $ parseModule fileName stream
  (<>) [(fileName, m)] <$> loadModules is

safeReadFile :: FilePath -> ExceptT String IO Text
safeReadFile filePath = do
  existsFile <- liftIO $ doesFileExist filePath
  if existsFile
    then liftIO $ T.readFile filePath
    else throwError ("does not exist: " <> filePath)
