{-# LANGUAGE ExistentialQuantification #-}

module Command.Daicker.Entry where

import Command.Daicker.Parser (parseArg)
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
import Language.Daicker.Parser (pModule, parse)
import Language.Daicker.Span (Span, mkSpan, spanPretty)
import Language.Daicker.StdLib (prelude)
import qualified Language.Daicker.Validator as V
import Language.LSP.Daicker (serve)
import Options.Applicative
import System.Directory (createDirectoryIfMissing)
import System.Exit (ExitCode (ExitFailure, ExitSuccess), exitFailure, exitSuccess, exitWith)
import System.FilePath ((</>))
import System.IO (Handle, IOMode (..), hClose, hGetContents, hIsClosed, hIsOpen, hPutStrLn, hReady, hWaitForInput, openFile, stderr, stdin, stdout)
import System.IO.Error.Lens (fileName)
import Text.Megaparsec (parseErrorPretty)

opts :: Parser (IO ())
opts =
  subparser
    ( command "serve" (info (pure $ void serve) (progDesc "Runs daicker language server."))
        <> command
          "check"
          ( info
              (check <$> fileOpt)
              (progDesc "Validates daicker file")
          )
        <> command
          "run"
          ( info
              ( run
                  <$> fileOpt
                  <*> argument str (metavar "FUNCTION")
                  <*> many (argument str (metavar "ARGS"))
              )
              (progDesc "Runs the specific function." <> noIntersperse)
          )
        <> command
          "eval"
          ( info
              ( eval
                  <$> fileOpt
                  <*> argument str (metavar "FUNCTION")
                  <*> many (argument str (metavar "ARGS"))
              )
              (progDesc "Evaluates the specific function and prints the return value to stdout." <> noIntersperse)
          )
    )

fileOpt :: Parser String
fileOpt =
  strOption
    ( long "file"
        <> short 'f'
        <> metavar "FILENAME"
        <> value "main.daic"
    )

check :: String -> IO ()
check fileName = do
  res <- runExceptT $ validate fileName
  case res of
    Right _ -> hPutStrLn stderr "The module is valid!"
    Left es -> hPutStrLn stderr $ staticErrorListPretty es

run :: String -> String -> [Text] -> IO ()
run fileName funcName args = do
  res <- runExceptT (run' fileName funcName args)
  case res of
    Left e -> hExitWithCodeErrors stderr e
    Right e -> withDevNull (`hExitWithExpr` e)

eval :: String -> String -> [Text] -> IO ()
eval fileName funcName args = do
  res <- runExceptT (run' fileName funcName args)
  case res of
    Left e -> hExitWithCodeErrors stderr e
    Right e -> hExitWithExpr stdout e

initialize :: IO ()
initialize = do
  createDirectoryIfMissing True ".daicker"
  createDirectoryIfMissing True (".daicker" </> "state")

validate :: String -> ExceptT [StaticError Span] IO ()
validate fileName = do
  src <- liftIO $ T.readFile fileName
  V.validate fileName src

readModule :: String -> ExceptT (CodeError Span) IO (Module Span)
readModule fileName = do
  src <- liftIO $ T.readFile fileName
  (m, _) <- liftEither $ mapLeft StaticE $ parse pModule fileName src
  pure m

run' :: String -> String -> [Text] -> ExceptT (CodeError Span) IO (Expr Span)
run' fileName funcName args = do
  src <- liftIO $ T.readFile fileName
  (m@(_ :< Module is _ ss), tokens) <- liftEither $ mapLeft StaticE $ parse pModule fileName src
  env <- withExceptT StaticE $ loadEnvironment m
  e <- case findExpr env funcName of
    Nothing -> throwError $ RuntimeE $ RuntimeError ("not found: " <> funcName) (mkSpan "command-line-function" 1 1 1 1) (ExitFailure 1)
    Just (_, e) -> pure e
  withExceptT StaticE $ liftEither $ V.validateModule env m
  case e of
    (s :< ELambda pms (s' :< e) t) -> do
      hasStdin <- liftIO $ hReady stdin
      input <- liftIO $ if hasStdin then Just <$> B.getContents else pure Nothing
      es <- liftEither $ mapLeft StaticE $ mapM (\(i, arg) -> parseArg ("command-line-argument($" <> show i <> ")") input arg) $ zip [1 ..] args
      withExceptT RuntimeE $ E.eval env (s :< ECall (s :< ELambda pms (s' :< e) t) es)
    _ -> withExceptT RuntimeE $ E.eval env e

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
