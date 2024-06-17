module Main where

import Control.Monad (join, void)
import Language.Daicker.DLS (serve)
import Language.Daicker.Executor (execDefine, findDefine)
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
  case syntaxCheck fileName src of
    [] -> pure ()
    es -> putStrLn $ errorBundlePretty $ head es

run :: String -> String -> IO ()
run fileName funcName = do
  src <- readFile fileName
  case parse pModule fileName src of
    Right m -> case findDefine funcName m of
      Just d -> execDefine d
      Nothing -> putStrLn $ "not define " <> funcName
    Left e -> putStrLn $ errorBundlePretty e

main :: IO ()
main = join $ execParser (info (opts <**> helper) idm)
