{-# LANGUAGE TupleSections #-}

module Language.Daicker.Entry where

import Control.Comonad.Cofree (Cofree (..))
import Control.Monad (liftM)
import Control.Monad.Except (ExceptT, MonadError (throwError), liftEither)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Identity (Identity)
import Data.Aeson (encode)
import Data.ByteString.Lazy.Char8 (unpack)
import qualified Data.ByteString.Lazy.Char8 as B
import Data.Sequence (mapWithIndex)
import Data.Text (Text)
import qualified Data.Text.IO as T
import Language.Daicker.AST (Expr, Expr' (EArray, EFun), Identifier, Module, Module' (..), Statement' (SDefine), switchAnn)
import Language.Daicker.CmdArgParser (parseArg)
import Language.Daicker.Error (CodeError (CodeError), RuntimeError (RuntimeError), codeErrorListPretty, runtimeErrorPretty)
import Language.Daicker.Executor (execDefine, findDefine)
import Language.Daicker.Lexer (lexTokens, mkTStreamWithoutComment)
import Language.Daicker.Parser (parseModule)
import Language.Daicker.Span (Span, mkSpan)
import Language.Daicker.TypeChecker (validateModule)
import System.Exit (ExitCode (ExitFailure, ExitSuccess), exitFailure, exitSuccess, exitWith)
import System.IO (Handle, IOMode (..), hClose, hGetContents, hPutStrLn, hReady, openFile, stdin)

validate :: String -> ExceptT [CodeError] IO ()
validate fileName = do
  src <- liftIO $ T.readFile fileName
  liftEither $ validate' fileName src

validate' :: String -> Text -> Either [CodeError] ()
validate' fileName src = do
  tokens <- liftEither $ lexTokens fileName src
  let stream = mkTStreamWithoutComment src tokens
  m <- liftEither $ parseModule fileName stream
  liftEither $ validateModule m

run :: String -> String -> [Text] -> ExceptT [CodeError] IO (Expr Span)
run fileName funcName args = do
  src <- liftIO $ T.readFile fileName
  tokens <- liftEither $ lexTokens fileName src
  let stream = mkTStreamWithoutComment src tokens
  m@(_ :< Module _ ss) <- liftEither $ parseModule fileName stream
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
