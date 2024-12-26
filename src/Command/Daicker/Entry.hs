{-# LANGUAGE ExistentialQuantification #-}

module Command.Daicker.Entry where

import Command.Daicker.Parser (RootCommand (..), pRootCommand)
import Command.Daicker.Parser.Argument (parseArg)
import Command.Daicker.Storage (readData, writeData)
import Control.Comonad.Cofree (Cofree (..))
import Control.Monad (join, liftM, void)
import Control.Monad.Except (ExceptT, MonadError (throwError), liftEither, runExceptT, withExceptT)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Identity (Identity)
import Data.Aeson (decode, encode)
import Data.Aeson.Types (Value)
import Data.ByteString.Lazy.Char8 (pack, unpack)
import qualified Data.ByteString.Lazy.Char8 as B
import Data.Sequence (mapWithIndex)
import Data.Text (Text)
import qualified Data.Text.IO as T
import GHC.IO.IOMode (IOMode (..))
import GHC.IO.StdHandles (openFile)
import Language.Daicker.AST (Argument, Argument' (..), Expr, Expr' (..), Identifier, Module, Module' (..), Statement' (SExpr), switchAnn)
import Language.Daicker.Bundler (findExpr, loadEnvironment)
import Language.Daicker.Error (CodeError (RuntimeE, StaticE), RuntimeError (RuntimeError), StaticError, codeErrorPretty, staticErrorListPretty)
import qualified Language.Daicker.Executor as E
import Language.Daicker.Parser (Token, pModule, parse)
import Language.Daicker.Span (Span, mkSpan, spanPretty)
import Language.Daicker.StdLib (prelude)
import qualified Language.Daicker.Validator as V
import Language.LSP.Daicker (serve)
import System.Directory (createDirectoryIfMissing)
import System.Exit (ExitCode (ExitFailure, ExitSuccess), exitFailure, exitSuccess, exitWith)
import System.FilePath ((</>))
import System.IO (Handle, IOMode (..), hClose, hGetContents, hIsClosed, hIsOpen, hPutStrLn, hReady, hWaitForInput, openFile, stderr, stdin, stdout)
import System.IO.Error.Lens (fileName)
import Text.Megaparsec (parseErrorPretty)

runRootCommand :: RootCommand -> IO ()
runRootCommand cmd = case cmd of
  Serve -> void serve
  Check fileName -> do
    res <- runExceptT $ validate fileName
    case res of
      Right _ -> hPutStrLn stderr "The module is valid!"
      Left es -> hPutStrLn stderr $ staticErrorListPretty es
  Run fileName funcName args -> do
    res <- runExceptT (evaluate fileName funcName args)
    case res of
      Left e -> hExitWithCodeErrors stderr e
      Right e -> withDevNull (`hExitWithExpr` e)
  Eval fileName funcName args -> do
    res <- runExceptT (evaluate fileName funcName args)
    case res of
      Left e -> hExitWithCodeErrors stderr e
      Right e -> hExitWithExpr stdout e
  InitState -> initialize
  GetState name -> hPutStrLn stderr "Not implemented"
  SetState name content -> hPutStrLn stderr "Not implemented"
  DeleteState name -> hPutStrLn stderr "Not implemented"

initialize :: IO ()
initialize = do
  createDirectoryIfMissing True ".daicker"
  createDirectoryIfMissing True (".daicker" </> "state")

validate :: String -> ExceptT [StaticError Span] IO ()
validate fileName = do
  src <- liftIO $ T.readFile fileName
  V.validate fileName src

evaluate :: String -> String -> [Text] -> ExceptT (CodeError Span) IO (Expr Span)
evaluate fileName funcName args = do
  (m@(_ :< Module is _ ss), tokens) <- readModule fileName
  env <- withExceptT StaticE $ loadEnvironment m
  e <- case findExpr env funcName of
    Nothing -> throwError $ RuntimeE $ RuntimeError ("not found: " <> funcName) (mkSpan "command-line-function" 1 1 1 1) (ExitFailure 1)
    Just (_, e) -> pure e
  withExceptT StaticE $ liftEither $ V.validateModule env m
  case e of
    (s :< ELambda pms (s' :< e) t) -> do
      input <- liftIO getStdin
      es <- liftEither $ mapLeft StaticE $ mapM (\(i, arg) -> parseArg ("command-line-argument($" <> show i <> ")") input arg) $ zip [1 ..] args
      withExceptT RuntimeE $ E.eval env (s :< ECall (s :< ELambda pms (s' :< e) t) es)
    _ -> withExceptT RuntimeE $ E.eval env e

getStdin :: IO (Maybe B.ByteString)
getStdin = do
  hasStdin <- hReady stdin
  if hasStdin then Just <$> B.getContents else pure Nothing

readModule :: String -> ExceptT (CodeError Span) IO (Module Span, [Token])
readModule fileName = do
  src <- liftIO $ T.readFile fileName
  liftEither $ mapLeft StaticE $ parse pModule fileName src

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
