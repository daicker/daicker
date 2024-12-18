module Main where

import Command.Daicker.Entry (opts)
import Control.Monad (join)
import Options.Applicative
import Options.Applicative.Builder (info)

main :: IO ()
main = join $ execParser (info (opts <**> helper) idm)
