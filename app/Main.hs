module Main where

import Control.Monad (join, void)
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
        <> command "valid" (info (valid <$> argument str idm) (progDesc "Valid"))
        <> command "run" (info (run <$> argument str idm <*> argument str idm) (progDesc "Execute"))
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

run :: String -> String -> IO ()
run fileName funcName = do
  src <- readFile fileName
  case mkTStream fileName src of
    Left e -> putStrLn $ errorBundlePretty e
    Right ts -> case parse pModule fileName ts of
      Left e -> putStrLn $ errorBundlePretty e
      Right m -> case findDefine funcName m of
        Nothing -> putStrLn $ "not defined: " <> funcName
        Just d -> case execDefine d of
          Left e -> print e
          Right e -> print e

main :: IO ()
main = join $ execParser (info (opts <**> helper) idm)
