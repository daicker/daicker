{-# LANGUAGE OverloadedStrings #-}

module Command.Daicker.Parser.Option where

import Data.ByteString.Lazy.Char8 (ByteString, pack)
import Data.Text (Text)
import Options.Applicative

data RootCommand
  = Serve
  | Check String
  | Run String String [Text]
  | Eval String String [Text]
  deriving (Show, Eq)

pRootCommand :: Parser RootCommand
pRootCommand =
  subparser
    ( command "serve" (info (pure Serve) (progDesc "Runs daicker language server."))
        <> command
          "check"
          ( info
              (Check <$> pFileOption)
              (progDesc "Validates daicker file")
          )
        <> command
          "run"
          ( info
              ( Run
                  <$> pFileOption
                  <*> argument str (metavar "FUNCTION")
                  <*> many (argument str (metavar "ARGS"))
              )
              (progDesc "Runs the specific function." <> noIntersperse)
          )
        <> command
          "eval"
          ( info
              ( Eval
                  <$> pFileOption
                  <*> argument str (metavar "FUNCTION")
                  <*> many (argument str (metavar "ARGS"))
              )
              (progDesc "Evaluates the specific function and prints the return value to stdout." <> noIntersperse)
          )
    )

pFileOption :: Parser String
pFileOption =
  strOption
    ( long "file"
        <> short 'f'
        <> metavar "FILENAME"
        <> value "main.daic"
    )
