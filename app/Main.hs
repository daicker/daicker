{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Comonad.Cofree
import Control.Monad (join, void)
import Control.Monad.Writer.Strict
import Data.Aeson (decode, encode)
import Data.Aeson.Types (Value)
import Data.ByteString.Lazy.Char8 (pack, unpack)
import Data.Maybe (fromMaybe)
import Language.Daicker.AST (Define' (Define), Expr, Expr' (EArray, EFun, ENull))
import Language.Daicker.DLS (serve)
import Language.Daicker.Executor (execDefine, findDefine)
import Language.Daicker.Lexer (mkTStream)
import Language.Daicker.Parser (pModule, syntaxCheck)
import Options.Applicative
import System.IO.Error.Lens (fileName)
import Text.Megaparsec (errorBundlePretty, parse, parseErrorPretty)

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
                  <$> argument str (metavar "FUNCTION")
                  <*> many (argument str (metavar "ARGS"))
                  <*> fileOpt
              )
              (progDesc "Execute function")
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
  case mkTStream fileName src of
    Left e -> putStrLn $ errorBundlePretty e
    Right ts ->
      case parse pModule fileName ts of
        Right m -> pure ()
        Left e -> putStrLn $ errorBundlePretty e

run :: String -> [String] -> String -> IO ()
run funcName args fileName = do
  src <- readFile fileName
  case mkTStream fileName src of
    Left e -> putStrLn $ errorBundlePretty e
    Right ts -> case parse pModule fileName ts of
      Left e -> putStrLn $ errorBundlePretty e
      Right m -> case findDefine funcName m of
        Nothing -> putStrLn $ "not defined: " <> funcName
        -- Requires an argument
        Just d@(_ :< Define _ (_ :< EFun (Just _) _)) -> do
          arg <- case args of
            [] -> do
              input <- getContents
              pure (decode (pack input) :: Maybe (Expr ()))
            [arg] -> do
              pure (decode (pack arg) :: Maybe (Expr ()))
            args -> do
              pure $ (:<) () . EArray <$> mapM (\arg -> decode (pack arg) :: Maybe (Expr ())) args
          case execDefine m d arg of
            Left e -> print e
            Right e -> putStrLn (unpack $ encode e)
        -- No argument
        Just d -> case execDefine m d Nothing of
          Left e -> print e
          Right e -> putStrLn (unpack $ encode e)

main :: IO ()
main = join $ execParser (info (opts <**> helper) idm)
