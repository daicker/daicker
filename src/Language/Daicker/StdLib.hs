{-# LANGUAGE OverloadedStrings #-}

module Language.Daicker.StdLib where

import Control.Comonad.Cofree (Cofree ((:<)))
import Control.Concurrent (putMVar, readMVar)
import Control.Concurrent.MVar (newEmptyMVar)
import Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString.Lazy.Char8 as B
import Data.List (intercalate)
import Data.Maybe (fromJust)
import qualified Data.Text as T
import Data.Text.IO (hGetLine, hPutStrLn)
import GHC.Conc (forkIO, par)
import GHC.IO.Exception (ExitCode (ExitFailure))
import GHC.IO.Handle (Handle)
import Language.Daicker.AST
import Language.Daicker.Span (Span (FixtureSpan), union)
import qualified Language.Daicker.Span as S
import Language.Daicker.StdLib.Prelude (mPrelude)
import Network.HTTP.Client
  ( Response (responseBody),
    httpLbs,
    newManager,
    parseRequest,
  )
import Network.HTTP.Client.TLS (tlsManagerSettings)
import System.Directory (getCurrentDirectory)
import System.Directory.Internal.Prelude (hClose)
import System.Exit (ExitCode (ExitSuccess))
import System.IO (hIsClosed, hIsEOF)
import qualified System.IO as IO
import System.Process (CreateProcess (..), createProcess, proc, shell, waitForProcess)
import System.Process.Common (showCreateProcessForUser)
import System.Process.Internals (StdStream (CreatePipe))

prelude :: Module Span
prelude = mPrelude

stdlib :: [(String, Module Span)]
stdlib = [("http", http)]

http :: Module Span
http =
  httpSpan
    :< Module
      []
      Nothing
      [ httpSpan
          :< SExpr
            (httpSpan :< Identifier "get")
            ( httpSpan
                :< EFixtureFun
                  [ httpSpan
                      :< PositionedParameter
                        (httpSpan :< Identifier "url")
                        False
                        False
                        (Just $ httpSpan :< TVar (httpSpan :< Identifier "String"))
                        Nothing
                  ]
                  ( \sp args -> do
                      let (_ :< EString url) = fromJust $ lookup "url" args
                      manager <- newManager tlsManagerSettings
                      request <- parseRequest url
                      response <- httpLbs request manager
                      let body = responseBody response
                      pure $ sp :< EString (B.unpack body)
                  )
                  (Just $ httpSpan :< TVar (httpSpan :< Identifier "String"))
            )
      ]
  where
    httpSpan :: Span
    httpSpan = FixtureSpan "http"
