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
  | InitState
  | GetState String
  | SetState String ByteString
  | DeleteState String
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
        <> command
          "state"
          ( info
              ( subparser
                  ( command "init" (info (pure InitState) (progDesc "Initializes the state."))
                      <> command
                        "get"
                        ( info
                            (GetState <$> argument str (metavar "STATE NAME"))
                            (progDesc "Get the specific state.")
                        )
                      <> command
                        "set"
                        ( info
                            ( SetState
                                <$> argument str (metavar "STATE NAME")
                                <*> argument str (metavar "STATE")
                            )
                            (progDesc "Set the specific state .")
                        )
                      <> command
                        "delete"
                        ( info
                            (DeleteState <$> argument str (metavar "STATE NAME"))
                            (progDesc "Delete the specific.")
                        )
                  )
              )
              (progDesc "Manages the state of the specific function.")
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
