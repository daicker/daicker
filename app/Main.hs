module Main where
import Language.Daicker.DLS (serve)
import Control.Monad (void, join)
import Options.Applicative
import Language.Daicker.Parser (syntaxCheck)
import System.IO.Error.Lens (fileName)
import Text.Megaparsec (parseErrorPretty, errorBundlePretty)

opts :: Parser (IO ())
opts = subparser
  ( command "serve" (info (pure $ void serve) (progDesc "Run daicker language server"))
  <> command "valid" (info (valid <$> argument str idm) (progDesc "Valid"))
  )

valid :: String -> IO ()
valid fileName = do
  src <- readFile fileName
  case syntaxCheck fileName src of
    [] -> pure ()
    es -> putStrLn $ errorBundlePretty $ head es

main :: IO ()
main = join $ execParser (info (opts <**> helper) idm)
