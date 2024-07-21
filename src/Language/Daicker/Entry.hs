module Language.Daicker.Entry where

import Control.Comonad.Cofree (Cofree (..))
import Control.Monad (liftM)
import Control.Monad.Except (ExceptT, MonadError (throwError), liftEither)
import Control.Monad.IO.Class (liftIO)
import Language.Daicker.AST (Expr, Expr' (EArray, EFun), Module, Module' (..), Statement' (SDefine), switchAnn)
import Language.Daicker.CmdArgParser (parseArg)
import Language.Daicker.Error (CodeError (CodeError))
import Language.Daicker.Executor (execDefine, findDefine)
import Language.Daicker.Parser (parseModule)
import Language.Daicker.Span (Span)
import Language.Daicker.TypeChecker (validateModule)
import System.IO (hGetContents, hReady, stdin)

validate :: String -> IO [CodeError]
validate fileName = do
  src <- readFile fileName
  pure $ sValidate fileName src

sValidate :: String -> String -> [CodeError]
sValidate fileName src = do
  case parseModule fileName src of
    Left es -> es
    Right m -> validateModule m

run :: String -> String -> [String] -> ExceptT [CodeError] IO (Expr Span)
run fileName funcName args = do
  src <- liftIO $ readFile fileName
  m@(_ :< Module _ ss) <- liftEither $ parseModule fileName src
  liftEither $ case validateModule m of
    [] -> Right ()
    es -> Left es
  (_ :< SDefine _ e _) <- case findDefine funcName ss of
    Nothing -> throwError [CodeError "" undefined]
    Just f -> return f
  hasStdin <- liftIO $ hReady stdin
  input <- liftIO $ if hasStdin then Just <$> getContents else pure Nothing
  es <- liftEither $ mapM (parseArg "command_line_arg" input) args
  liftEither $ execDefine m e (() :< EArray (map (switchAnn (const ())) es))
