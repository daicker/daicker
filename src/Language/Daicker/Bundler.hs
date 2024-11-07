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
    Statement,
    Statement' (SExpr, SType),
    Type,
    URL,
    URL' (LocalFile),
  )
import Language.Daicker.Error (RuntimeError, StaticError (StaticError))
import Language.Daicker.Parser (pModule, parse)
import Language.Daicker.Span (Span)
import Language.Daicker.StdLib (prelude, stdlib)
import System.Directory (doesFileExist)
import System.IO (readFile)

data Bundle a = Bundle
  { prelude :: Module a,
    modules :: ModuleBundle a,
    current :: Module a,
    statements :: StatementBundle a,
    arguments :: [(String, Expr a)]
  }
  deriving (Show, Eq)

type StatementBundle a = [(String, (Statement a, Module a))]

type ModuleBundle a = [(String, Module a)]

findExpr :: String -> [Statement a] -> Maybe (Statement a)
findExpr n ss = find (\s@(_ :< SExpr (_ :< Identifier n') _) -> n' == n) $ filter isExpr ss

findType :: String -> [Statement a] -> Maybe (Statement a)
findType n ss = find (\s@(_ :< SType (_ :< Identifier n') _ _) -> n' == n) $ filter isType ss

findExprWithBundle :: Bundle a -> Identifier a -> Either [StaticError a] (Expr a, Bundle a)
findExprWithBundle b@(Bundle prelude ms cm ss as) (s :< Identifier name) =
  case lookup name as of
    Just e -> Right (e, b)
    Nothing -> case lookup name (filter (\(_, (s, _)) -> isExpr s) ss) of
      Just (_ :< SExpr _ e, m) -> do
        ss' <- loadStatements prelude ms m
        pure (e, Bundle prelude ms m ss' as)
      Nothing -> Left [StaticError ("not defined: " <> name) s]

loadStatements :: Module a -> ModuleBundle a -> Module a -> Either [StaticError a] (StatementBundle a)
loadStatements prelude ms m@(s :< Module is e ss) = do
  let ss' = map (toPairStatement m) ss
  bs <- mapM (importedStatements ms) is
  ps <- exportedStatements prelude
  pure $ ps <> join bs <> ss'

isExpr :: Statement a -> Bool
isExpr (_ :< SExpr {}) = True
isExpr _ = False

isType :: Statement a -> Bool
isType (_ :< SType {}) = True
isType _ = False

importedStatements :: ModuleBundle a -> Import a -> Either [StaticError a] (StatementBundle a)
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

exportedStatements :: Module a -> Either [StaticError a] (StatementBundle a)
exportedStatements m@(s :< Module _ export ss) = case export of
  Just (_ :< Export names) -> mapM findExpr names
  Nothing -> pure $ map (toPairStatement m) ss
  where
    exprs = map (toPairStatement m) ss
    findExpr i@(s :< Identifier name) = case lookup name exprs of
      Nothing -> Left [StaticError ("not defined: " <> name) s]
      Just e -> Right (name, e)

toPairStatement :: Module a -> Statement a -> (String, (Statement a, Module a))
toPairStatement m s@(_ :< SExpr (_ :< Identifier name) _) = (name, (s, m))
toPairStatement m s@(_ :< SType (_ :< Identifier name) _ _) = (name, (s, m))

loadModules :: [Import Span] -> ExceptT [StaticError Span] IO (ModuleBundle Span)
loadModules = foldr (\i -> (<*>) ((<>) <$> importModules i)) (pure [])

importModules :: Import Span -> ExceptT [StaticError Span] IO (ModuleBundle Span)
importModules (s :< PartialImport _ url) = readModule url
importModules (s :< WildImport url) = readModule url

readModule :: URL Span -> ExceptT [StaticError Span] IO (ModuleBundle Span)
readModule (s :< LocalFile fileName) = do
  case lookup fileName stdlib of
    Just m -> pure [(fileName, m)]
    Nothing -> do
      src <- withExceptT (\e -> [StaticError e s]) $ safeReadFile fileName
      (m@(_ :< Module is _ _), tokens) <- liftEither $ parse pModule fileName src
      (<>) [(fileName, m)] <$> loadModules is

safeReadFile :: FilePath -> ExceptT String IO Text
safeReadFile filePath = do
  existsFile <- liftIO $ doesFileExist filePath
  if existsFile
    then liftIO $ T.readFile filePath
    else throwError ("does not exist: " <> filePath)
