{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad (join, void)
import Control.Monad.Except (runExceptT)
import Data.Aeson (decode, encode)
import Data.Aeson.Types (Value)
import Data.ByteString.Lazy.Char8 (pack, unpack)
import Data.Text (Text)
import GHC.IO.IOMode (IOMode (..))
import GHC.IO.StdHandles (openFile)
import Language.Daicker.DLS (serve)
import Language.Daicker.Entry (hExitWithCodeErrors, hExitWithExpr, withDevNull)
import qualified Language.Daicker.Entry as E
import Language.Daicker.Error (staticErrorListPretty)
import Language.Daicker.Parser (pModule, parse)
import Options.Applicative
import System.IO (hClose, hGetContents, hIsClosed, hIsOpen, hPutStrLn, hReady, hWaitForInput, stderr, stdin, stdout)
import System.IO.Error.Lens (fileName)
import Text.Megaparsec (parse, parseErrorPretty)

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
  res <- runExceptT $ E.validate fileName
  case res of
    Right _ -> hPutStrLn stderr "The module is valid!"
    Left es -> hPutStrLn stderr $ staticErrorListPretty es

run :: String -> String -> [Text] -> IO ()
run fileName funcName args = do
  res <- runExceptT (E.run fileName funcName args)
  case res of
    Left e -> hExitWithCodeErrors stderr e
    Right e -> withDevNull (`hExitWithExpr` e)

eval :: String -> String -> [Text] -> IO ()
eval fileName funcName args = do
  res <- runExceptT (E.run fileName funcName args)
  case res of
    Left e -> hExitWithCodeErrors stderr e
    Right e -> hExitWithExpr stdout e

main :: IO ()
main = join $ execParser (info (opts <**> helper) idm)
