module Main where
import Language.Daicker.DLS (serve)
import Control.Monad (void, join)
import Options.Applicative

opts :: Parser (IO ())
opts = subparser
  ( command "serve" (info (pure $ void serve) (progDesc "Run daicker language server") ) )

main :: IO ()
main = join $ execParser (info (opts <**> helper) idm)
