module Language.Daicker.Storage where

import Control.Comonad.Cofree (Cofree ((:<)))
import Control.Monad.Except (ExceptT, MonadError (throwError))
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Aeson (Value, decode, encode, (.:))
import qualified Data.ByteString.Lazy.Char8 as B
import GHC.IO.Exception (ExitCode (ExitFailure))
import Language.Daicker.AST (Data, Data' (LocalState), Expr, Identifier' (Identifier), switchAnn)
import Language.Daicker.Error (RuntimeError (RuntimeError))
import Language.Daicker.Span (Span (DataSpan))
import System.FilePath ((<.>), (</>))

readData :: Data Span -> ExceptT RuntimeError IO (Expr Span)
readData (_ :< LocalState (s :< Identifier name)) = do
  stateRaw <- liftIO $ B.readFile (".daicker" </> "state" </> name <.> "json")
  case decode stateRaw :: Maybe (Expr ()) of
    Just e -> pure (switchAnn (const $ DataSpan (name <> " (local state)")) e)
    Nothing -> throwError $ RuntimeError "" s (ExitFailure 1)

writeData :: Data Span -> Expr Span -> IO ()
writeData (_ :< LocalState (s :< Identifier name)) e =
  B.writeFile (".daicker" </> "state" </> name <.> "json") (encode e)
