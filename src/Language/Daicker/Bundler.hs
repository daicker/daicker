module Language.Daicker.Bundler where

import Control.Comonad.Cofree (Cofree ((:<)))
import Control.Exception (IOException, catch)
import Control.Monad (join)
import Control.Monad.Error.Class (MonadError (throwError), liftEither)
import Control.Monad.Except (ExceptT, withExceptT)
import Control.Monad.IO.Class (MonadIO (..))
import Data.List (find)
import Data.Text (Text)
import qualified Data.Text.IO as T
import Language.Daicker.AST (Expr, Identifier' (Identifier), Import, Import' (NamedImport, PartialImport, WildImport), Module, Module' (Module), Statement, Statement' (SDefine, STypeDefine), URL, URL' (LocalFile))
import Language.Daicker.Error (StaticError (StaticError))
import Language.Daicker.Lexer (lexTokens, mkTStreamWithoutComment)
import Language.Daicker.Parser (parseModule)
import Language.Daicker.Span (Span)

findDefine :: String -> [Statement a] -> Maybe (Statement a)
findDefine n = find (\s -> isDefine s && name s == n)
  where
    isDefine (_ :< SDefine {}) = True
    isDefine _ = False
    name (_ :< SDefine (_ :< Identifier n) _ _) = n

findType :: String -> [Statement a] -> Maybe (Statement a)
findType n = find (\s -> isType s && name s == n)
  where
    isType (_ :< STypeDefine {}) = True
    isType _ = False
    name (_ :< STypeDefine (_ :< Identifier n) _) = n

data Bundle = Bundle String (Expr Span) [Bundle]

loadExprs :: Module Span -> ExceptT [StaticError] IO Bundle
loadExprs (s :< Module is e ss) = undefined

loadModules :: [Import Span] -> ExceptT [StaticError] IO [(String, Module Span)]
loadModules = foldr (\i -> (<*>) ((<>) <$> importModules i)) (pure [])

importModules :: Import Span -> ExceptT [StaticError] IO [(String, Module Span)]
importModules (s :< NamedImport _ url) = readModule url
importModules (s :< PartialImport _ url) = readModule url
importModules (s :< WildImport url) = readModule url

readModule :: URL Span -> ExceptT [StaticError] IO [(String, Module Span)]
readModule (s :< LocalFile fileName) = do
  src <- withExceptT (\e -> [StaticError (show e) s]) $ safeReadFile fileName
  tokens <- liftEither $ lexTokens fileName src
  let stream = mkTStreamWithoutComment src tokens
  m@(_ :< Module is _ _) <- liftEither $ parseModule fileName stream
  (<>) [(fileName, m)] <$> loadModules is

safeReadFile :: FilePath -> ExceptT IOException IO Text
safeReadFile filePath = liftIO $ T.readFile filePath `catch` throwError
