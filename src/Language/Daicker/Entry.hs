{-# LANGUAGE ExistentialQuantification #-}

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
import Language.Daicker.AST (Argument, Argument' (..), Expr, Expr' (..), Identifier, Module, Module' (..), Statement' (SExpr), switchAnn)
import Language.Daicker.Bundler (findExpr, loadEnvironment)
import Language.Daicker.CmdArgParser (parseArg)
import Language.Daicker.Error (CodeError (RuntimeE, StaticE), RuntimeError (RuntimeError), StaticError, codeErrorPretty)
import Language.Daicker.Executor (eval)
import Language.Daicker.Parser (pModule, parse)
import Language.Daicker.Span (Span, mkSpan, spanPretty)
import Language.Daicker.StdLib (prelude)
import Language.Daicker.Validator (validateModule)
import System.Directory (createDirectoryIfMissing)
import System.Exit (ExitCode (ExitFailure, ExitSuccess), exitFailure, exitSuccess, exitWith)
import System.FilePath ((</>))
import System.IO (Handle, IOMode (..), hClose, hGetContents, hPutStrLn, hReady, openFile, stdin)

initialize :: IO ()
initialize = do
  createDirectoryIfMissing True ".daicker"
  createDirectoryIfMissing True (".daicker" </> "state")

validate :: String -> ExceptT [StaticError Span] IO ()
validate fileName = do
  src <- liftIO $ T.readFile fileName
  validate' fileName src

validate' :: String -> Text -> ExceptT [StaticError Span] IO ()
validate' fileName src = do
  (m@(_ :< Module is _ _), _) <- liftEither $ parse pModule fileName src
  env <- loadEnvironment m
  liftEither $ validateModule env m

readModule :: String -> ExceptT (CodeError Span) IO (Module Span)
readModule fileName = do
  src <- liftIO $ T.readFile fileName
  (m, _) <- liftEither $ mapLeft StaticE $ parse pModule fileName src
  pure m

run :: String -> String -> [Text] -> ExceptT (CodeError Span) IO (Expr Span)
run fileName funcName args = do
  src <- liftIO $ T.readFile fileName
  (m@(_ :< Module is _ ss), tokens) <- liftEither $ mapLeft StaticE $ parse pModule fileName src
  env <- withExceptT StaticE $ loadEnvironment m
  e <- case findExpr env funcName of
    Nothing -> throwError $ RuntimeE $ RuntimeError ("not found: " <> funcName) (mkSpan "command-line-function" 1 1 1 1) (ExitFailure 1)
    Just (_, e) -> pure e
  withExceptT StaticE $ liftEither $ validateModule env m
  case e of
    (s :< ELambda pms (s' :< e) t) -> do
      hasStdin <- liftIO $ hReady stdin
      input <- liftIO $ if hasStdin then Just <$> B.getContents else pure Nothing
      es <- liftEither $ mapLeft StaticE $ mapM (\(i, arg) -> parseArg ("command-line-argument($" <> show i <> ")") input arg) $ zip [1 ..] args
      withExceptT RuntimeE $ eval env (s :< ECall (s :< ELambda pms (s' :< e) t) es)
    _ -> withExceptT RuntimeE $ eval env e

mapLeft :: (a -> c) -> Either a b -> Either c b
mapLeft f = either (Left . f) Right

hExitWithCodeErrors :: Handle -> CodeError Span -> IO ()
hExitWithCodeErrors h e = do
  hPutStrLn h $ codeErrorPretty e
  exitFailure

hExitWithRuntimeError :: Handle -> CodeError Span -> IO ()
hExitWithRuntimeError h e@(RuntimeE (RuntimeError m s n)) = do
  hPutStrLn h $ codeErrorPretty e
  exitWith n

hExitWithExpr :: Handle -> Expr Span -> IO ()
hExitWithExpr h e = do
  case e of
    (s :< EError m code) -> do
      hPutStrLn h $ spanPretty s <> ": " <> m
      exitWith code
    e -> do
      hPutStrLn h (unpack $ encode e)
      exitSuccess

withDevNull :: (Handle -> IO a) -> IO a
withDevNull f = do
  handle <- openFile "/dev/null" WriteMode
  res <- f handle
  hClose handle
  return res
