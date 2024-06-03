module Main where
import Language.Daicker.DLS (serve)
import Control.Monad (void, join)
import Options.Applicative
import Language.Daicker.Parser (syntaxCheck)

opts :: Parser (IO ())
opts = subparser
  ( command "serve" (info (pure $ void serve) (progDesc "Run daicker language server"))
  <> command "valid" (info (syntaxCheck <$> argument str idm) (progDesc "Valid"))
  )

main :: IO ()
main = join $ execParser (info (opts <**> helper) idm)
