{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}

module Language.Daicker.DLS where

import Colog.Core (LogAction (..), Severity (..), WithSeverity (..), (<&))
import qualified Colog.Core as L
import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.STM.TChan
import Control.Lens hiding (Iso)
import Control.Monad (forever)
import Control.Monad.IO.Class
import qualified Data.Aeson as J
import qualified Data.List.NonEmpty as NEL
import qualified Data.Text as T
import Data.Void (Void)
import GHC.Generics (Generic)
import Language.Daicker.Parser (syntaxCheck)
import Language.LSP.Diagnostics
import Language.LSP.Logging (defaultClientLogger)
import qualified Language.LSP.Protocol.Lens as LSP
import Language.LSP.Protocol.Message ()
import qualified Language.LSP.Protocol.Message as LSP
import Language.LSP.Protocol.Types
import qualified Language.LSP.Protocol.Types as LSP
import Language.LSP.Server
import Language.LSP.VFS (VirtualFile (VirtualFile), virtualFileText, virtualFileVersion)
import Text.Megaparsec (ParseErrorBundle (bundlePosState), PosState (PosState), SourcePos (SourcePos), bundlePosState, errorBundlePretty, unPos)
import Text.Megaparsec.Error (ParseErrorBundle (bundleErrors), errorOffset)
import Text.Megaparsec.State (PosState (pstateSourcePos))
import Text.Megaparsec.Stream (reachOffset)

data Config = Config {fooTheBar :: Bool, wibbleFactor :: Int}
  deriving (Generic, Show)

instance J.ToJSON Config where
  toEncoding = J.genericToEncoding J.defaultOptions

instance J.FromJSON Config

sendDiagnostics :: LSP.NormalizedUri -> Maybe Int32 -> [ParseErrorBundle String Void] -> LspM Config ()
sendDiagnostics fileUri version es = do
  let diags =
        map
          ( \e -> do
              let (SourcePos _ l c) = errorBundleSourcePos e
              LSP.Diagnostic
                ( LSP.mkRange
                    (fromIntegral $ unPos l - 1)
                    (fromIntegral $ unPos c - 1)
                    (fromIntegral $ unPos l - 1)
                    (fromIntegral $ unPos c)
                )
                (Just LSP.DiagnosticSeverity_Error) -- severity
                Nothing -- code
                Nothing
                (Just "daicker") -- source
                (T.pack $ errorBundlePretty e)
                Nothing -- tags
                (Just [])
                Nothing
          )
          es
  publishDiagnostics 100 fileUri version (partitionBySource diags)

handle :: (m ~ LspM Config) => L.LogAction m (WithSeverity T.Text) -> Handlers m
handle logger =
  mconcat
    [ notificationHandler LSP.SMethod_Initialized $ \_msg -> do
        logger <& "Processing the Initialized notification" `WithSeverity` Info,
      notificationHandler LSP.SMethod_TextDocumentDidOpen $ \msg -> do
        let doc =
              msg
                ^. LSP.params
                  . LSP.textDocument
                  . LSP.uri
                  . to LSP.toNormalizedUri
        let (NormalizedUri _ path) = doc
        logger <& ("Processing DidOpenTextDocument for: " <> T.pack (show doc)) `WithSeverity` Info
        mdoc <- getVirtualFile doc
        case mdoc of
          Just file -> sendDiagnostics doc (Just $ virtualFileVersion file) $ syntaxCheck (T.unpack path) (T.unpack $ virtualFileText file)
          Nothing -> sendDiagnostics doc Nothing [],
      notificationHandler LSP.SMethod_TextDocumentDidChange $ \msg -> do
        let doc =
              msg
                ^. LSP.params
                  . LSP.textDocument
                  . LSP.uri
                  . to LSP.toNormalizedUri
        let (NormalizedUri _ path) = doc
        mdoc <- getVirtualFile doc
        logger <& ("Processing TextDocumentDidChange for: " <> T.pack (show doc)) `WithSeverity` Info
        case mdoc of
          Just file -> sendDiagnostics doc (Just $ virtualFileVersion file) $ syntaxCheck (T.unpack path) (T.unpack $ virtualFileText file)
          Nothing -> sendDiagnostics doc Nothing [],
      notificationHandler LSP.SMethod_TextDocumentDidSave $ \msg -> do
        let doc =
              msg
                ^. LSP.params
                  . LSP.textDocument
                  . LSP.uri
                  . to LSP.toNormalizedUri
        let (NormalizedUri _ path) = doc
        logger <& ("Processing DidSaveTextDocument  for: " <> T.pack (show doc)) `WithSeverity` Info
        mdoc <- getVirtualFile doc
        case mdoc of
          Just file -> sendDiagnostics doc (Just $ virtualFileVersion file) $ syntaxCheck (T.unpack path) (T.unpack $ virtualFileText file)
          Nothing -> sendDiagnostics doc Nothing []
    ]

newtype ReactorInput
  = ReactorAction (IO ())

serve :: IO Int
serve = do
  let stderrLogger :: LogAction IO (WithSeverity T.Text)
      stderrLogger = L.cmap show L.logStringStderr
      clientLogger :: LogAction (LspM Config) (WithSeverity T.Text)
      clientLogger = defaultClientLogger
      dualLogger :: LogAction (LspM Config) (WithSeverity T.Text)
      dualLogger = clientLogger <> L.hoistLogAction liftIO stderrLogger

  rin <- atomically newTChan :: IO (TChan ReactorInput)
  runServer $
    ServerDefinition
      { defaultConfig = Config {fooTheBar = False, wibbleFactor = 0},
        parseConfig = \_old v -> do
          case J.fromJSON v of
            J.Error e -> Left (T.pack e)
            J.Success cfg -> Right cfg,
        onConfigChange = const $ pure (),
        configSection = "demo",
        doInitialize = \env _ -> forkIO (reactor stderrLogger rin) >> pure (Right env),
        -- Handlers log to both the client and stderr
        staticHandlers = \_caps -> lspHandlers dualLogger rin,
        interpretHandler = \env -> Iso (runLspT env) liftIO,
        options = lspOptions
      }

syncOptions :: LSP.TextDocumentSyncOptions
syncOptions =
  LSP.TextDocumentSyncOptions
    { LSP._openClose = Just True,
      LSP._change = Just LSP.TextDocumentSyncKind_Incremental,
      LSP._willSave = Just False,
      LSP._willSaveWaitUntil = Just False,
      LSP._save = Just $ LSP.InR $ LSP.SaveOptions $ Just False
    }

lspOptions :: Options
lspOptions =
  defaultOptions
    { optTextDocumentSync = Just syncOptions,
      optExecuteCommandCommands = Just ["lsp-hello-command"]
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

errorBundleSourcePos :: ParseErrorBundle String Void -> SourcePos
errorBundleSourcePos peb = do
  let pst = bundlePosState peb
  let e = NEL.head $ bundleErrors peb
  let (_, pst') = reachOffset (errorOffset e) pst
  pstateSourcePos pst'
