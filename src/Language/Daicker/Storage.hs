module Language.Daicker.Storage where

import Control.Monad.Except (ExceptT, MonadError (throwError))
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Aeson (Value, decode, encode, (.:))
import qualified Data.ByteString.Lazy.Char8 as B
import GHC.IO.Exception (ExitCode (ExitFailure))
import Language.Daicker.AST (Expr)
import Language.Daicker.Error (RuntimeError (RuntimeError))
import System.FilePath ((<.>), (</>))

stateDir :: FilePath
stateDir = ".daicker" </> "state"

readData :: FilePath -> ExceptT (RuntimeError ()) IO (Expr ())
readData name = do
  stateRaw <- liftIO $ B.readFile (stateDir </> name <.> "json")
  case decode stateRaw :: Maybe (Expr ()) of
    Just e -> pure e
    Nothing -> throwError $ RuntimeError ("Cannot read data: " <> name) () (ExitFailure 1)

writeData :: String -> Expr () -> IO ()
writeData name e =
  B.writeFile (stateDir </> name <.> "json") (encode e)
