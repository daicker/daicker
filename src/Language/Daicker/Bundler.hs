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
  ( Export' (Export),
    Expr,
    Identifier,
    Identifier' (Identifier),
    Import,
    Import' (PartialImport, WildImport),
    Module,
    Module' (Module),
    NamedStatement,
    NamedStatement' (NamedStatement),
    Statement,
    Statement' (SExpr, SType),
    Type,
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

data Bundle a = Bundle
  { modules :: ModuleBundle a,
    current :: Module a,
    statements :: StatementBundle a
  }
  deriving (Show, Eq)

type StatementBundle a = [(String, (Statement a, Module a))]

type ModuleBundle a = [(String, Module a)]

findExpr :: String -> [NamedStatement a] -> Maybe (NamedStatement a)
findExpr n = find (\(_ :< NamedStatement (_ :< Identifier n') s) -> isExpr s && n' == n)

findType :: String -> [NamedStatement a] -> Maybe (NamedStatement a)
findType n = find (\(_ :< NamedStatement (_ :< Identifier n') s) -> isType s && n' == n)

findExprWithBundle :: Bundle Span -> Identifier Span -> Either [StaticError] (Expr Span, Bundle Span)
findExprWithBundle (Bundle ms cm ss) (s :< Identifier name) = case lookup name (filter (\(_, (s, _)) -> isExpr s) ss) of
  Just (_ :< SExpr e, m) -> do
    ss' <- loadStatements ms m
    pure (e, Bundle ms m ss')
  Nothing -> Left [StaticError ("not defined: " <> name) s]

loadStatements :: ModuleBundle Span -> Module Span -> Either [StaticError] (StatementBundle Span)
loadStatements ms m@(s :< Module is e ss) = do
  let ss' = map (toPairStatement m) ss
  bs <- mapM (importedStatements ms) is
  ps <- exportedStatements prelude
  pure $ ps <> join bs <> ss'

pickupType :: [Statement a] -> [Type a]
pickupType ss = map (\(_ :< SType d) -> d) $ filter isType ss

pickupExpr :: [Statement a] -> [Expr a]
pickupExpr ss = map (\(_ :< SExpr d) -> d) $ filter isExpr ss

isExpr :: Statement a -> Bool
isExpr (_ :< SExpr {}) = True
isExpr _ = False

isType :: Statement a -> Bool
isType (_ :< SType {}) = True
isType _ = False

importedStatements :: ModuleBundle Span -> Import Span -> Either [StaticError] (StatementBundle Span)
importedStatements ms (s :< impt) = do
  case impt of
    PartialImport imports (_ :< LocalFile url) -> do
      let m = findModule url
      ss <- exportedStatements m
      mapM (findExpr ss) imports
    WildImport (_ :< LocalFile url) -> do
      let m = findModule url
      exprs <- exportedStatements m
      pure (map (\(k, (e, _)) -> (k, (e, findModule url))) exprs)
  where
    findModule url = fromJust $ lookup url ms
    findExpr exprs i@(s :< Identifier name) = case lookup name exprs of
      Nothing -> Left [StaticError ("not defined: " <> name) s]
      Just e -> Right (name, e)

exportedStatements :: Module Span -> Either [StaticError] (StatementBundle Span)
exportedStatements m@(s :< Module _ export ss) = case export of
  Just (_ :< Export names) -> mapM findExpr names
  Nothing -> pure $ map (toPairStatement m) ss
  where
    exprs = map (toPairStatement m) ss
    findExpr i@(s :< Identifier name) = case lookup name exprs of
      Nothing -> Left [StaticError ("not defined: " <> name) s]
      Just e -> Right (name, e)

toPairStatement :: Module a -> NamedStatement a -> (String, (Statement a, Module a))
toPairStatement m (_ :< NamedStatement (_ :< Identifier name) t) = (name, (t, m))

loadModules :: [Import Span] -> ExceptT [StaticError] IO (ModuleBundle Span)
loadModules = foldr (\i -> (<*>) ((<>) <$> importModules i)) (pure [])

importModules :: Import Span -> ExceptT [StaticError] IO (ModuleBundle Span)
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
