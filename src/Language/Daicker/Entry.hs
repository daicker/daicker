{-# LANGUAGE TupleSections #-}

module Language.Daicker.Entry where

import Control.Comonad.Cofree (Cofree (..))
import Control.Monad (liftM)
import Control.Monad.Except (ExceptT, MonadError (throwError), liftEither)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (encode)
import Data.ByteString.Lazy.Char8 (unpack)
import qualified Data.ByteString.Lazy.Char8 as B
import Data.Sequence (mapWithIndex)
import Data.Text (Text)
import qualified Data.Text.IO as T
import Language.Daicker.AST (Expr, Expr' (EArray, EFun), Module, Module' (..), Statement' (SDefine), switchAnn)
import Language.Daicker.CmdArgParser (parseArg)
import Language.Daicker.Error (CodeError (CodeError), RuntimeError (RuntimeError), codeErrorListPretty, runtimeErrorPretty)
import Language.Daicker.Executor (execDefine, findDefine)
import Language.Daicker.Parser (parseModule)
import Language.Daicker.Span (Span, mkSpan)
import Language.Daicker.TypeChecker (validateModule)
import System.Exit (ExitCode (ExitFailure, ExitSuccess), exitFailure, exitSuccess, exitWith)
import System.IO (Handle, IOMode (..), hClose, hGetContents, hPutStrLn, hReady, openFile, stdin)

validate :: String -> IO [CodeError]
validate fileName = do
  src <- T.readFile fileName
  pure $ sValidate fileName src

sValidate :: String -> Text -> [CodeError]
sValidate fileName src = do
  case parseModule fileName src of
    Left es -> es
    Right m -> validateModule m

run :: String -> String -> [Text] -> ExceptT [CodeError] IO (Expr Span)
run fileName funcName args = do
  src <- liftIO $ T.readFile fileName
  m@(_ :< Module _ ss) <- liftEither $ parseModule fileName src
  liftEither $ case validateModule m of
    [] -> Right ()
    es -> Left es
  (_ :< SDefine _ e _) <- case findDefine funcName ss of
    Nothing -> throwError [CodeError ("not found: " <> funcName) (mkSpan "command-line-function" 1 1 1 1)]
    Just f -> return f
  hasStdin <- liftIO $ hReady stdin
  input <- liftIO $ if hasStdin then Just <$> B.getContents else pure Nothing
  es <- liftEither $ mapM (\(i, arg) -> parseArg ("command-line-argument($" <> show i <> ")") input arg) $ zip [1 ..] args
  liftEither $ execDefine m e (map (,False) es)

hExitWithCodeErrors :: Handle -> [CodeError] -> IO ()
hExitWithCodeErrors h e = do
  hPutStrLn h $ codeErrorListPretty e
  exitFailure

hExitWithRuntimeError :: Handle -> RuntimeError -> IO ()
hExitWithRuntimeError h e@(RuntimeError m s n) = do
  hPutStrLn h $ runtimeErrorPretty e
  exitWith n

hExitWithExpr :: Handle -> Expr a -> IO ()
hExitWithExpr h e = do
  hPutStrLn h (unpack $ encode e)
  exitSuccess

withDevNull :: (Handle -> IO a) -> IO a
withDevNull f = do
  handle <- openFile "/dev/null" WriteMode
  res <- f handle
  hClose handle
  return res
