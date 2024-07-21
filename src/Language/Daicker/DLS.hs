{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
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
import Data.Maybe (catMaybes, isJust)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Void (Void)
import GHC.Generics (Generic)
import Language.Daicker.Entry (sValidate)
import Language.Daicker.Error (CodeError (CodeError))
import Language.Daicker.Lexer
import Language.Daicker.Span (Span (Span), WithSpan (WithSpan), toRange)
import qualified Language.Daicker.Span as S
import Language.LSP.Diagnostics
import Language.LSP.Logging (defaultClientLogger)
import Language.LSP.Protocol.Lens (HasSemanticTokens (semanticTokens))
import qualified Language.LSP.Protocol.Lens as LSP
import Language.LSP.Protocol.Message ()
import qualified Language.LSP.Protocol.Message as LSP
import Language.LSP.Protocol.Types
import qualified Language.LSP.Protocol.Types as LSP
import Language.LSP.Server
import Language.LSP.VFS (VirtualFile (VirtualFile), virtualFileText, virtualFileVersion)
import Text.Megaparsec (ParseErrorBundle (bundlePosState), PosState (PosState), SourcePos (SourcePos), bundlePosState, choice, errorBundlePretty, parse, unPos)
import Text.Megaparsec.Error (ParseErrorBundle (bundleErrors), errorOffset)
import Text.Megaparsec.State (PosState (pstateSourcePos))
import Text.Megaparsec.Stream (reachOffset)

data Config = Config {fooTheBar :: Bool, wibbleFactor :: Int}
  deriving (Generic, Show)

instance J.ToJSON Config where
  toEncoding = J.genericToEncoding J.defaultOptions

instance J.FromJSON Config

sendDiagnostics :: LSP.NormalizedUri -> Maybe Int32 -> [CodeError] -> LspM Config ()
sendDiagnostics fileUri version es = do
  let diags =
        map
          ( \(CodeError m s) -> do
              LSP.Diagnostic
                (toRange s)
                (Just LSP.DiagnosticSeverity_Error) -- severity
                Nothing -- code
                Nothing
                (Just "daicker") -- source
                (T.pack m)
                Nothing -- tags
                (Just [])
                Nothing
          )
          es
  publishDiagnostics 100 fileUri version (partitionBySource diags)

sendSyntaxError ::
  (m ~ LspM Config, LSP.HasUri a1 Uri, LSP.HasTextDocument a2 a1, LSP.HasParams s a2) =>
  L.LogAction m (WithSeverity T.Text) ->
  s ->
  LspT Config IO ()
sendSyntaxError logger msg = do
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
    Just file -> sendDiagnostics doc (Just $ virtualFileVersion file) $ sValidate (T.unpack path) (T.unpack $ virtualFileText file)
    Nothing -> sendDiagnostics doc Nothing []

handle :: (m ~ LspM Config) => L.LogAction m (WithSeverity T.Text) -> Handlers m
handle logger =
  mconcat
    [ notificationHandler LSP.SMethod_Initialized $ \_msg -> do
        logger <& "Processing the Initialized notification" `WithSeverity` Info,
      notificationHandler LSP.SMethod_TextDocumentDidOpen $ sendSyntaxError logger,
      notificationHandler LSP.SMethod_TextDocumentDidChange $ sendSyntaxError logger,
      notificationHandler LSP.SMethod_TextDocumentDidSave $ sendSyntaxError logger,
      requestHandler LSP.SMethod_TextDocumentSemanticTokensFull $ \req responder -> do
        let doc =
              req
                ^. LSP.params
                  . LSP.textDocument
                  . LSP.uri
                  . to LSP.toNormalizedUri
        mdoc <- getVirtualFile doc
        let (NormalizedUri _ path) = doc
        let tokens = case mdoc of
              Just file -> case lexSemanticTokens (T.unpack path) (T.unpack $ virtualFileText file) of
                Right ts -> makeSemanticTokens defaultSemanticTokensLegend ts
                Left e -> Left e
              Nothing -> makeSemanticTokens defaultSemanticTokensLegend []
        case tokens of
          Left t -> responder $ Left $ LSP.ResponseError (LSP.InR LSP.ErrorCodes_InternalError) t Nothing
          Right tokens -> responder $ Right $ LSP.InL tokens,
      notificationHandler LSP.SMethod_WorkspaceDidChangeConfiguration $ \_ -> pure () -- Nothing to do
    ]

newtype ReactorInput = ReactorAction (IO ())

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

lexSemanticTokens :: String -> String -> Either Text [SemanticTokenAbsolute]
lexSemanticTokens fileName src =
  case parse tTokens fileName src of
    Left e -> Left $ T.pack $ errorBundlePretty e
    Right ts -> Right $ makeSemanticTokenAbsolutes ts
  where
    makeSemanticTokenAbsolutes :: [WithSpan TToken] -> [SemanticTokenAbsolute]
    makeSemanticTokenAbsolutes [] = []
    makeSemanticTokenAbsolutes (WithSpan x (Span (S.Position _ l1 c1) (S.Position _ l2 c2)) : ts) =
      case toSemanticTokenTypes x of
        Just t ->
          SemanticTokenAbsolute
            (fromIntegral $ l1 - 1)
            (fromIntegral $ c1 - 1)
            (fromIntegral $ c2 - c1)
            t
            []
            : makeSemanticTokenAbsolutes ts
        Nothing -> makeSemanticTokenAbsolutes ts
    toSemanticTokenTypes :: TToken -> Maybe SemanticTokenTypes
    toSemanticTokenTypes t = case t of
      TNull -> Just SemanticTokenTypes_Macro
      TBool _ -> Just SemanticTokenTypes_Macro
      TNumber _ -> Just SemanticTokenTypes_Number
      TString _ -> Just SemanticTokenTypes_String
      TModule -> Just SemanticTokenTypes_Keyword
      TImport -> Just SemanticTokenTypes_Keyword
      TExport -> Just SemanticTokenTypes_Keyword
      TDefine -> Just SemanticTokenTypes_Keyword
      TType -> Just SemanticTokenTypes_Keyword
      TIdentifier _ -> Just SemanticTokenTypes_Variable
      TImage _ -> Just SemanticTokenTypes_Macro
      TComment -> Just SemanticTokenTypes_Comment
      _ -> Nothing
