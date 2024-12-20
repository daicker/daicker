module Main where

import Command.Daicker.Entry (runRootCommand)
import Command.Daicker.Parser (pRootCommand)
import Control.Monad (join)
import Options.Applicative
import Options.Applicative.Builder (info)

main :: IO ()
main = join $ execParser (info (runRootCommand <$> pRootCommand <**> helper) idm)
