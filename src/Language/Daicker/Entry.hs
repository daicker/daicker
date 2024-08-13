{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}

module Language.Daicker.Entry where

import Control.Comonad.Cofree (Cofree (..))
import Control.Monad (liftM, void)
import Control.Monad.Except (ExceptT, MonadError (throwError), liftEither, runExceptT, withExceptT)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Identity (Identity)
import Data.Aeson (encode)
import Data.ByteString.Lazy.Char8 (unpack)
import qualified Data.ByteString.Lazy.Char8 as B
import Data.Sequence (mapWithIndex)
import Data.Text (Text)
import qualified Data.Text.IO as T
import Language.Daicker.AST (Expr, Expr' (EArray, EFun), Identifier, Module, Module' (..), NamedStatement' (NamedStatement), Statement' (SExpr), switchAnn)
import Language.Daicker.Bundler (Bundle (Bundle), findExpr, loadModules, loadStatements)
import Language.Daicker.CmdArgParser (parseArg)
import Language.Daicker.Error (CodeError (RuntimeE, StaticE), RuntimeError (RuntimeError), StaticError, codeErrorPretty)
import Language.Daicker.Executor (execDefine)
import Language.Daicker.Lexer (lexTokens, mkTStreamWithoutComment)
import Language.Daicker.Parser (parseModule)
import Language.Daicker.Span (Span, mkSpan)
import Language.Daicker.TypeChecker (validateModule)
import System.Exit (ExitCode (ExitFailure, ExitSuccess), exitFailure, exitSuccess, exitWith)
import System.IO (Handle, IOMode (..), hClose, hGetContents, hPutStrLn, hReady, openFile, stdin)

validate :: String -> ExceptT [StaticError] IO ()
validate fileName = do
  src <- liftIO $ T.readFile fileName
  validate' fileName src

validate' :: String -> Text -> ExceptT [StaticError] IO ()
validate' fileName src = do
  tokens <- liftEither $ lexTokens fileName src
  let stream = mkTStreamWithoutComment src tokens
  m@(_ :< Module is _ _) <- liftEither $ parseModule fileName stream
  mb <- loadModules is
  ss <- liftEither $ loadStatements mb m
  liftEither $ validateModule (Bundle mb m ss) m
  ms <- loadModules is
  void $ liftEither $ loadStatements ms m

run :: String -> String -> [Text] -> ExceptT CodeError IO (Expr Span)
run fileName funcName args = do
  src <- liftIO $ T.readFile fileName
  tokens <- liftEither $ mapLeft StaticE $ lexTokens fileName src
  let stream = mkTStreamWithoutComment src tokens
  m@(_ :< Module is _ ss) <- liftEither $ mapLeft StaticE $ parseModule fileName stream
  e <- case findExpr funcName ss of
    Nothing -> throwError $ RuntimeE $ RuntimeError ("not found: " <> funcName) (mkSpan "command-line-function" 1 1 1 1) (ExitFailure 1)
    Just (_ :< NamedStatement _ (_ :< SExpr e)) -> return e
  mb <- withExceptT StaticE $ loadModules is
  ss <- withExceptT StaticE $ liftEither $ loadStatements mb m
  withExceptT StaticE $ liftEither $ validateModule (Bundle mb m ss) m
  hasStdin <- liftIO $ hReady stdin
  input <- liftIO $ if hasStdin then Just <$> B.getContents else pure Nothing
  es <- liftEither $ mapLeft StaticE $ mapM (\(i, arg) -> parseArg ("command-line-argument($" <> show i <> ")") input arg) $ zip [1 ..] args
  withExceptT RuntimeE $ execDefine (Bundle mb m ss) e (map (,False) es)

mapLeft :: (a -> c) -> Either a b -> Either c b
mapLeft f = either (Left . f) Right

hExitWithCodeErrors :: Handle -> CodeError -> IO ()
hExitWithCodeErrors h e = do
  hPutStrLn h $ codeErrorPretty e
  exitFailure

hExitWithRuntimeError :: Handle -> CodeError -> IO ()
hExitWithRuntimeError h e@(RuntimeE (RuntimeError m s n)) = do
  hPutStrLn h $ codeErrorPretty e
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
