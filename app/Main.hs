{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Comonad.Cofree
import Control.Monad (join, void)
import Control.Monad.Writer.Strict
import Data.Aeson (decode, encode)
import Data.Aeson.Types (Value)
import Data.ByteString.Lazy.Char8 (pack, unpack)
import Data.Maybe (fromMaybe)
import Language.Daicker.AST (Expr, Expr' (EArray, EFun, ENull, EString), Statement' (SDefine))
import Language.Daicker.DLS (serve)
import Language.Daicker.Error (codeErrorListPretty, codeErrorPretty)
import Language.Daicker.Executor (execDefine, findDefine)
import Language.Daicker.Lexer (mkTStream)
import Language.Daicker.Parser (pModule, parseModule, syntaxCheck)
import Options.Applicative
import System.IO.Error.Lens (fileName)
import Text.Megaparsec (parse, parseErrorPretty)

opts :: Parser (IO ())
opts =
  subparser
    ( command "serve" (info (pure $ void serve) (progDesc "Run daicker language server"))
        <> command
          "valid"
          ( info
              (valid <$> fileOpt)
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
    )

fileOpt :: Parser String
fileOpt =
  strOption
    ( long "file"
        <> short 'f'
        <> metavar "FILENAME"
        <> value "main.daic"
    )

valid :: String -> IO ()
valid fileName = do
  src <- readFile fileName
  case parseModule fileName src of
    Right m -> pure ()
    Left e -> putStrLn $ codeErrorListPretty e

run :: String -> String -> [String] -> IO ()
run fileName funcName args = do
  src <- readFile fileName
  case parseModule fileName src of
    Left e -> putStrLn $ codeErrorListPretty e
    Right m -> case findDefine funcName m of
      Nothing -> putStrLn $ "not defined: " <> funcName
      -- Requires an argument
      Just e@(_ :< EFun (Just _) _) -> do
        arg <- case args of
          [] -> do
            input <- getContents
            pure (decode (pack input) :: Maybe (Expr ()))
          args -> do
            pure $ Just $ () :< EArray (map (\arg -> () :< EString arg) args)
        case execDefine m e arg of
          Left e -> putStrLn $ codeErrorListPretty e
          Right e -> putStrLn (unpack $ encode e)
      -- No argument
      Just d -> case execDefine m d Nothing of
        Left e -> putStrLn $ codeErrorListPretty e
        Right e -> putStrLn (unpack $ encode e)

main :: IO ()
main = join $ execParser (info (opts <**> helper) idm)
