module Language.Daicker.Bundler where

import Control.Comonad.Cofree (Cofree ((:<)))
import Control.Exception.Safe (IOException, catch)
import Control.Monad (join)
import Control.Monad.Error.Class (MonadError (throwError), liftEither)
import Control.Monad.Except (ExceptT, withExceptT)
import Control.Monad.IO.Class (MonadIO (..))
import Data.List (find)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Language.Daicker.AST
  ( Define,
    Define'
      ( Define
      ),
    Expr,
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
import Language.Daicker.Error (StaticError (StaticError))
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

data ExprBundle = ExprBundle String (Expr Span) [ExprBundle] deriving (Show, Eq)

toExprBundle :: (String, Expr Span) -> ExprBundle
toExprBundle (name, e) = ExprBundle name e []

lookupExpr :: String -> [ExprBundle] -> Maybe (Expr Span)
lookupExpr name bundles = expr <$> find (\(ExprBundle name' _ _) -> name == name') bundles
  where
    expr (ExprBundle _ e _) = e

loadExprs :: Module Span -> ExceptT [StaticError] IO [ExprBundle]
loadExprs (s :< Module is e ss) = do
  let es = map toPairExpr (pickupExpr ss)
  ms <- loadModules is
  bs <- join <$> mapM loadExprs ms
  pure $ map (\(s, e) -> ExprBundle s e bs) (es <> prelude)

toPairExpr :: Define a -> (String, Expr a)
toPairExpr (_ :< Define (_ :< Identifier name) e _) = (name, e)

pickupExpr :: [Statement a] -> [Define a]
pickupExpr ss = map (\(_ :< SDefine d) -> d) $ filter isExpr ss

isExpr :: Statement a -> Bool
isExpr (_ :< SDefine {}) = True
isExpr _ = False

isType :: Statement a -> Bool
isType (_ :< STypeDefine {}) = True
isType _ = False

loadModules :: [Import Span] -> ExceptT [StaticError] IO [Module Span]
loadModules = foldr (\i -> (<*>) ((<>) <$> importModules i)) (pure [])

importModules :: Import Span -> ExceptT [StaticError] IO [Module Span]
importModules (s :< NamedImport _ url) = readModule url
importModules (s :< PartialImport _ url) = readModule url
importModules (s :< WildImport url) = readModule url

readModule :: URL Span -> ExceptT [StaticError] IO [Module Span]
readModule (s :< LocalFile fileName) = do
  src <- withExceptT (\e -> [StaticError e s]) $ safeReadFile fileName
  tokens <- liftEither $ lexTokens fileName src
  let stream = mkTStreamWithoutComment src tokens
  m@(_ :< Module is _ _) <- liftEither $ parseModule fileName stream
  (<>) [m] <$> loadModules is

safeReadFile :: FilePath -> ExceptT String IO Text
safeReadFile filePath = do
  existsFile <- liftIO $ doesFileExist filePath
  if existsFile
    then liftIO $ T.readFile filePath
    else throwError "does not exist daic file"
