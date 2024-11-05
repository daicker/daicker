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
import Language.Daicker.AST (Expr, Expr' (EArray, EError, ELambda), Identifier, Module, Module' (..), Statement' (SExpr), switchAnn)
import Language.Daicker.Bundler (Bundle (Bundle), StatementBundle, findExpr, loadModules, loadStatements)
import Language.Daicker.CmdArgParser (parseArg)
import Language.Daicker.Error (CodeError (RuntimeE, StaticE), RuntimeError (RuntimeError), StaticError, codeErrorPretty)
import Language.Daicker.Executor (eval)
import Language.Daicker.Parser (pModule, parse)
import Language.Daicker.Span (Span, mkSpan, spanPretty)
import Language.Daicker.TypeChecker (validateModule)
import System.Directory (createDirectoryIfMissing)
import System.Exit (ExitCode (ExitFailure, ExitSuccess), exitFailure, exitSuccess, exitWith)
import System.FilePath ((</>))
import System.IO (Handle, IOMode (..), hClose, hGetContents, hPutStrLn, hReady, openFile, stdin)

initialize :: IO ()
initialize = do
  createDirectoryIfMissing True ".daicker"
  createDirectoryIfMissing True (".daicker" </> "state")

validate :: String -> ExceptT [StaticError] IO ()
validate fileName = do
  src <- liftIO $ T.readFile fileName
  validate' fileName src

validate' :: String -> Text -> ExceptT [StaticError] IO ()
validate' fileName src = do
  (m@(_ :< Module is _ _), _) <- liftEither $ parse pModule fileName src
  mb <- loadModules is
  ss <- liftEither $ loadStatements mb m
  liftEither $ validateModule (Bundle mb m ss) m
  ms <- loadModules is
  void $ liftEither $ loadStatements ms m

statements' :: String -> Text -> ExceptT [StaticError] IO (StatementBundle Span)
statements' fileName src = do
  (m@(_ :< Module is _ _), _) <- liftEither $ parse pModule fileName src
  mb <- loadModules is
  liftEither $ loadStatements mb m

run :: String -> String -> [Text] -> ExceptT CodeError IO (Expr Span)
run fileName funcName args = do
  src <- liftIO $ T.readFile fileName
  (m@(_ :< Module is _ ss), tokens) <- liftEither $ mapLeft StaticE $ parse pModule fileName src
  e <- case findExpr funcName ss of
    Nothing -> throwError $ RuntimeE $ RuntimeError ("not found: " <> funcName) (mkSpan "command-line-function" 1 1 1 1) (ExitFailure 1)
    Just (_ :< SExpr _ e) -> return e
  mb <- withExceptT StaticE $ loadModules is
  ss <- withExceptT StaticE $ liftEither $ loadStatements mb m
  withExceptT StaticE $ liftEither $ validateModule (Bundle mb m ss) m
  hasStdin <- liftIO $ hReady stdin
  input <- liftIO $ if hasStdin then Just <$> B.getContents else pure Nothing
  es <- liftEither $ mapLeft StaticE $ mapM (\(i, arg) -> parseArg ("command-line-argument($" <> show i <> ")") input arg) $ zip [1 ..] args
  withExceptT RuntimeE $ eval (Bundle mb m ss) e -- TODO: pass arguments

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
