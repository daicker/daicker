{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Comonad.Cofree
import Control.Monad (join, void)
import Control.Monad.Except (runExceptT)
import Control.Monad.Writer.Strict
import Data.Aeson (decode, encode)
import Data.Aeson.Types (Value)
import Data.ByteString.Lazy.Char8 (pack, unpack)
import Data.Maybe (fromMaybe)
import GHC.IO.IOMode (IOMode (..))
import GHC.IO.StdHandles (openFile)
import Language.Daicker.AST (Expr, Expr' (EArray, EFun, ENull, EString), Module, Module' (Module), Statement' (SDefine))
import Language.Daicker.DLS (serve)
import Language.Daicker.Entry (hExitWithCodeErrors, hExitWithExpr, withDevNull)
import qualified Language.Daicker.Entry as E
import Language.Daicker.Error (codeErrorListPretty, codeErrorPretty)
import Language.Daicker.Executor (execDefine, findDefine)
import Language.Daicker.Lexer (mkTStream)
import Language.Daicker.Parser (pModule, parseModule)
import Options.Applicative
import System.IO (hClose, hGetContents, hIsClosed, hIsOpen, hPutStrLn, hReady, hWaitForInput, stderr, stdin, stdout)
import System.IO.Error.Lens (fileName)
import Text.Megaparsec (parse, parseErrorPretty)

opts :: Parser (IO ())
opts =
  subparser
    ( command "serve" (info (pure $ void serve) (progDesc "Run daicker language server"))
        <> command
          "check"
          ( info
              (validate <$> fileOpt)
              (progDesc "Validate daicker file")
          )
        <> command
          "run"
          ( info
              ( run
                  <$> fileOpt
                  <*> argument str (metavar "FUNCTION")
                  <*> many (argument str (metavar "ARGS"))
              )
              (progDesc "Execute function" <> noIntersperse)
          )
        <> command
          "eval"
          ( info
              ( eval
                  <$> fileOpt
                  <*> argument str (metavar "FUNCTION")
                  <*> many (argument str (metavar "ARGS"))
              )
              (progDesc "Execute function" <> noIntersperse)
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

validate :: String -> IO ()
validate fileName = do
  res <- E.validate fileName
  case res of
    [] -> hPutStrLn stderr "The module is valid!"
    es -> hPutStrLn stderr $ codeErrorListPretty es

run :: String -> String -> [String] -> IO ()
run fileName funcName args = do
  res <- runExceptT (E.run fileName funcName args)
  case res of
    Left e -> hExitWithCodeErrors stderr e
    Right e -> withDevNull (`hExitWithExpr` e)

eval :: String -> String -> [String] -> IO ()
eval fileName funcName args = do
  res <- runExceptT (E.run fileName funcName args)
  case res of
    Left e -> hExitWithCodeErrors stderr e
    Right e -> hExitWithExpr stdout e

main :: IO ()
main = join $ execParser (info (opts <**> helper) idm)
