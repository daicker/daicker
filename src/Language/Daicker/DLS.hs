{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}

module Language.Daicker.DLS where

import Colog.Core (LogAction (..), Severity (..), WithSeverity (..), (<&))
import qualified Colog.Core as L
import Control.Monad.IO.Class
import Control.Lens hiding (Iso)
import Control.Concurrent
import Control.Concurrent.STM.TChan
import Data.Text as T
import qualified Data.Aeson as J
import Language.LSP.Protocol.Message ()
import Language.LSP.Protocol.Types
import Language.LSP.Server
import GHC.Generics (Generic)
import qualified Language.LSP.Protocol.Lens as LSP
import Language.LSP.Logging (defaultClientLogger)
import qualified Language.LSP.Protocol.Message as LSP
import qualified Language.LSP.Protocol.Types as LSP
import Language.LSP.Diagnostics
import Control.Concurrent.STM
import Control.Monad (forever)

data Config = Config {fooTheBar :: Bool, wibbleFactor :: Int}
  deriving (Generic, Show)

instance J.ToJSON Config where
  toEncoding = J.genericToEncoding J.defaultOptions

instance J.FromJSON Config where

sendDiagnostics :: LSP.NormalizedUri -> Maybe Int32 -> LspM Config ()
sendDiagnostics fileUri version = do
  let
    diags =
      [ LSP.Diagnostic
          (LSP.Range (LSP.Position 0 1) (LSP.Position 0 5))
          (Just LSP.DiagnosticSeverity_Warning) -- severity
          Nothing -- code
          Nothing
          (Just "lsp-hello") -- source
          "Example diagnostic message"
          Nothing -- tags
          (Just [])
          Nothing
      ]
  publishDiagnostics 100 fileUri version (partitionBySource diags)

handle :: (m ~ LspM Config) => L.LogAction m (WithSeverity T.Text) -> Handlers m
handle logger =
  mconcat
    [ notificationHandler LSP.SMethod_TextDocumentDidOpen $ \msg -> do
        let doc = msg ^. LSP.params . LSP.textDocument . LSP.uri
            fileName = LSP.uriToFilePath doc
        logger <& ("Processing DidOpenTextDocument for: " <> T.pack (show fileName)) `WithSeverity` Info
        sendDiagnostics (LSP.toNormalizedUri doc) (Just 0)
    , notificationHandler LSP.SMethod_TextDocumentDidSave $ \msg -> do
        let doc = msg ^. LSP.params . LSP.textDocument . LSP.uri
            fileName = LSP.uriToFilePath doc
        logger <& ("Processing DidSaveTextDocument  for: " <> T.pack (show fileName)) `WithSeverity` Info
        sendDiagnostics (LSP.toNormalizedUri doc) Nothing
    ]

newtype ReactorInput
  = ReactorAction (IO ())

serve :: IO Int
serve = do
  let
    stderrLogger :: LogAction IO (WithSeverity T.Text)
    stderrLogger = L.cmap show L.logStringStderr
    clientLogger :: LogAction (LspM Config) (WithSeverity T.Text)
    clientLogger = defaultClientLogger
    dualLogger :: LogAction (LspM Config) (WithSeverity T.Text)
    dualLogger = clientLogger <> L.hoistLogAction liftIO stderrLogger

  rin <- atomically newTChan :: IO (TChan ReactorInput)
  runServer $
    ServerDefinition
        { defaultConfig = Config{fooTheBar = False, wibbleFactor = 0}
        , parseConfig = \_old v -> do
            case J.fromJSON v of
              J.Error e -> Left (T.pack e)
              J.Success cfg -> Right cfg
        , onConfigChange = const $ pure ()
        , configSection = "demo"
        , doInitialize = \env _ -> forkIO (reactor stderrLogger rin) >> pure (Right env)
        , -- Handlers log to both the client and stderr
          staticHandlers = \_caps -> lspHandlers dualLogger rin
        , interpretHandler = \env -> Iso (runLspT env) liftIO
        , options = lspOptions
        }

syncOptions :: LSP.TextDocumentSyncOptions
syncOptions =
  LSP.TextDocumentSyncOptions
    { LSP._openClose = Just True
    , LSP._change = Just LSP.TextDocumentSyncKind_Incremental
    , LSP._willSave = Just False
    , LSP._willSaveWaitUntil = Just False
    , LSP._save = Just $ LSP.InR $ LSP.SaveOptions $ Just False
    }

lspOptions :: Options
lspOptions =
  defaultOptions
    { optTextDocumentSync = Just syncOptions
    , optExecuteCommandCommands = Just ["lsp-hello-command"]
    }

lspHandlers :: (m ~ LspM Config) => L.LogAction m (WithSeverity T.Text) -> TChan ReactorInput -> Handlers m
lspHandlers logger rin = mapHandlers goReq goNot (handle logger)
 where
  goReq :: forall (a :: LSP.Method LSP.ClientToServer LSP.Request). Handler (LspM Config) a -> Handler (LspM Config) a
  goReq f msg k = do
    env <- getLspEnv
    liftIO $ atomically $ writeTChan rin $ ReactorAction (runLspT env $ f msg k)
  goNot :: forall (a :: LSP.Method LSP.ClientToServer LSP.Notification). Handler (LspM Config) a -> Handler (LspM Config) a
  goNot f msg = do
    env <- getLspEnv
    liftIO $ atomically $ writeTChan rin $ ReactorAction (runLspT env $ f msg)

reactor :: L.LogAction IO (WithSeverity T.Text) -> TChan ReactorInput -> IO ()
reactor logger inp = do
  logger <& "Started the reactor" `WithSeverity` Info
  forever $ do
    ReactorAction act <- atomically $ readTChan inp
    act
