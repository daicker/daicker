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
prelude =
  preludeSpan
    :< Module
      []
      Nothing
      [ preludeSpan
          :< SExpr
            (preludeSpan :< Identifier "$_")
            ( preludeSpan
                :< EFixtureFun
                  [ preludeSpan
                      :< PositionedParameter
                        (preludeSpan :< Identifier "script")
                        False
                        False
                        (Just $ preludeSpan :< TVar (preludeSpan :< Identifier "String"))
                        Nothing,
                    preludeSpan
                      :< KeywordParameter
                        (preludeSpan :< Identifier "image")
                        False
                        False
                        (Just $ preludeSpan :< TVar (preludeSpan :< Identifier "String"))
                        (Just $ preludeSpan :< EImage (preludeSpan :< Identifier "local"))
                  ]
                  ( \sp args -> do
                      let (_ :< EString script) = fromJust $ lookup "script" args
                      let (_ :< EImage (_ :< Identifier image)) = fromJust $ lookup "image" args
                      (CommandResult i out err) <- liftIO $ case image of
                        "local" -> runSubprocess "local" script
                        image -> runContainer image script
                      pure $
                        sp
                          :< EObject
                            [ (sp :< EString "exitCode", sp :< ENumber (fromIntegral $ exitCodeToInt i)),
                              (sp :< EString "stdout", sp :< EString out),
                              (sp :< EString "stderr", sp :< EString err)
                            ]
                  )
                  ( Just $
                      preludeSpan
                        :< TObject
                          [ (preludeSpan :< TStringLiteral "exitCode", preludeSpan :< TVar (preludeSpan :< Identifier "Number")),
                            (preludeSpan :< TStringLiteral "stdout", preludeSpan :< TVar (preludeSpan :< Identifier "String")),
                            (preludeSpan :< TStringLiteral "stderr", preludeSpan :< TVar (preludeSpan :< Identifier "String"))
                          ]
                  )
            ),
        preludeSpan
          :< SExpr
            (preludeSpan :< Identifier "$")
            ( preludeSpan
                :< EFixtureFun
                  [ preludeSpan
                      :< PositionedParameter
                        (preludeSpan :< Identifier "script")
                        False
                        False
                        (Just $ preludeSpan :< TVar (preludeSpan :< Identifier "String"))
                        Nothing,
                    preludeSpan
                      :< KeywordParameter
                        (preludeSpan :< Identifier "image")
                        False
                        False
                        (Just $ preludeSpan :< TVar (preludeSpan :< Identifier "String"))
                        (Just $ preludeSpan :< EImage (preludeSpan :< Identifier "local"))
                  ]
                  ( \sp args -> do
                      let (_ :< EString script) = fromJust $ lookup "script" args
                      let (_ :< EImage (_ :< Identifier image)) = fromJust $ lookup "image" args
                      (CommandResult i out err) <- liftIO $ case image of
                        "local" -> runSubprocess "local" script
                        image -> runContainer image script
                      case i of
                        ExitSuccess -> pure $ sp :< EString out
                        ExitFailure _ -> pure $ sp :< EError ("Error command exec: " <> "$ " <> script) i
                  )
                  (Just $ preludeSpan :< TVar (preludeSpan :< Identifier "String"))
            ),
        preludeSpan
          :< SExpr
            (preludeSpan :< Identifier "$1")
            ( preludeSpan
                :< EFixtureFun
                  [ preludeSpan
                      :< PositionedParameter
                        (preludeSpan :< Identifier "script")
                        False
                        False
                        (Just $ preludeSpan :< TVar (preludeSpan :< Identifier "String"))
                        Nothing,
                    preludeSpan
                      :< KeywordParameter
                        (preludeSpan :< Identifier "image")
                        False
                        False
                        (Just $ preludeSpan :< TVar (preludeSpan :< Identifier "String"))
                        (Just $ preludeSpan :< EImage (preludeSpan :< Identifier "local"))
                  ]
                  ( \sp args -> do
                      let (_ :< EString script) = fromJust $ lookup "script" args
                      let (_ :< EImage (_ :< Identifier image)) = fromJust $ lookup "image" args
                      (CommandResult i out err) <- liftIO $ case image of
                        "local" -> runSubprocess "local" script
                        image -> runContainer image script
                      case i of
                        ExitSuccess -> pure $ sp :< EString out
                        ExitFailure _ -> pure $ sp :< EError ("Error command exec: " <> "$1 " <> script) i
                  )
                  (Just $ preludeSpan :< TVar (preludeSpan :< Identifier "String"))
            ),
        preludeSpan
          :< SExpr
            (preludeSpan :< Identifier "$2")
            ( preludeSpan
                :< EFixtureFun
                  [ preludeSpan
                      :< PositionedParameter
                        (preludeSpan :< Identifier "script")
                        False
                        False
                        (Just $ preludeSpan :< TVar (preludeSpan :< Identifier "String"))
                        Nothing,
                    preludeSpan
                      :< KeywordParameter
                        (preludeSpan :< Identifier "image")
                        False
                        False
                        (Just $ preludeSpan :< TVar (preludeSpan :< Identifier "String"))
                        (Just $ preludeSpan :< EImage (preludeSpan :< Identifier "local"))
                  ]
                  ( \sp args -> do
                      let (_ :< EString script) = fromJust $ lookup "script" args
                      let (_ :< EImage (_ :< Identifier image)) = fromJust $ lookup "image" args
                      (CommandResult i out err) <- liftIO $ case image of
                        "local" -> runSubprocess "local" script
                        image -> runContainer image script
                      case i of
                        ExitSuccess -> pure $ sp :< EString err
                        ExitFailure _ -> pure $ sp :< EError ("Error command exec: " <> "$1 " <> script) i
                  )
                  (Just $ preludeSpan :< TVar (preludeSpan :< Identifier "String"))
            ),
        preludeSpan
          :< SExpr
            (preludeSpan :< Identifier ">>")
            ( preludeSpan
                :< EFixtureFun
                  [ preludeSpan :< PositionedParameter (preludeSpan :< Identifier "a") False False Nothing Nothing,
                    preludeSpan :< PositionedParameter (preludeSpan :< Identifier "b") False False Nothing Nothing
                  ]
                  ( \sp args -> do
                      let (_ :< e1) = fromJust $ lookup "a" args
                      let (_ :< e2) = fromJust $ lookup "b" args
                      _ <- pure e1 -- execute forcibly
                      pure $ sp :< e2
                  )
                  Nothing
            ),
        preludeSpan
          :< SExpr
            (preludeSpan :< Identifier "|>")
            ( preludeSpan
                :< EFixtureFun
                  [ preludeSpan :< PositionedParameter (preludeSpan :< Identifier "a") False False Nothing Nothing,
                    preludeSpan :< PositionedParameter (preludeSpan :< Identifier "b") False False Nothing Nothing
                  ]
                  ( \sp args -> do
                      let e1 = fromJust $ lookup "a" args
                      let e2 = fromJust $ lookup "b" args
                      pure $
                        sp
                          :< ECall
                            e1
                            [sp :< PositionedArgument e2]
                  )
                  Nothing
            ),
        preludeSpan
          :< SExpr
            (preludeSpan :< Identifier "+")
            ( preludeSpan
                :< EFixtureFun
                  [ preludeSpan
                      :< PositionedParameter
                        (preludeSpan :< Identifier "a")
                        False
                        False
                        (Just $ preludeSpan :< TVar (preludeSpan :< Identifier "Number"))
                        Nothing,
                    preludeSpan
                      :< PositionedParameter
                        (preludeSpan :< Identifier "b")
                        False
                        False
                        (Just $ preludeSpan :< TVar (preludeSpan :< Identifier "Number"))
                        Nothing
                  ]
                  ( \sp args -> do
                      let (_ :< ENumber a) = fromJust $ lookup "a" args
                      let (_ :< ENumber b) = fromJust $ lookup "b" args
                      pure $ sp :< ENumber (a + b)
                  )
                  (Just $ preludeSpan :< TVar (preludeSpan :< Identifier "Number"))
            ),
        preludeSpan
          :< SExpr
            (preludeSpan :< Identifier "-")
            ( preludeSpan
                :< EFixtureFun
                  [ preludeSpan
                      :< PositionedParameter
                        (preludeSpan :< Identifier "a")
                        False
                        False
                        (Just $ preludeSpan :< TVar (preludeSpan :< Identifier "Number"))
                        Nothing,
                    preludeSpan
                      :< PositionedParameter
                        (preludeSpan :< Identifier "b")
                        False
                        False
                        (Just $ preludeSpan :< TVar (preludeSpan :< Identifier "Number"))
                        Nothing
                  ]
                  ( \sp args -> do
                      let (_ :< ENumber a) = fromJust $ lookup "a" args
                      let (_ :< ENumber b) = fromJust $ lookup "b" args
                      pure $ sp :< ENumber (a - b)
                  )
                  (Just $ preludeSpan :< TVar (preludeSpan :< Identifier "Number"))
            ),
        preludeSpan
          :< SExpr
            (preludeSpan :< Identifier "*")
            ( preludeSpan
                :< EFixtureFun
                  [ preludeSpan
                      :< PositionedParameter
                        (preludeSpan :< Identifier "a")
                        False
                        False
                        (Just $ preludeSpan :< TVar (preludeSpan :< Identifier "Number"))
                        Nothing,
                    preludeSpan
                      :< PositionedParameter
                        (preludeSpan :< Identifier "b")
                        False
                        False
                        (Just $ preludeSpan :< TVar (preludeSpan :< Identifier "Number"))
                        Nothing
                  ]
                  ( \sp args -> do
                      let (_ :< ENumber a) = fromJust $ lookup "a" args
                      let (_ :< ENumber b) = fromJust $ lookup "b" args
                      pure $ sp :< ENumber (a * b)
                  )
                  (Just $ preludeSpan :< TVar (preludeSpan :< Identifier "Number"))
            ),
        preludeSpan
          :< SExpr
            (preludeSpan :< Identifier "/")
            ( preludeSpan
                :< EFixtureFun
                  [ preludeSpan
                      :< PositionedParameter
                        (preludeSpan :< Identifier "a")
                        False
                        False
                        (Just $ preludeSpan :< TVar (preludeSpan :< Identifier "Number"))
                        Nothing,
                    preludeSpan
                      :< PositionedParameter
                        (preludeSpan :< Identifier "b")
                        False
                        False
                        (Just $ preludeSpan :< TVar (preludeSpan :< Identifier "Number"))
                        Nothing
                  ]
                  ( \sp args -> do
                      let (_ :< ENumber a) = fromJust $ lookup "a" args
                      let (_ :< ENumber b) = fromJust $ lookup "b" args
                      pure $ sp :< ENumber (a / b)
                  )
                  (Just $ preludeSpan :< TVar (preludeSpan :< Identifier "Number"))
            )
      ]
  where
    exitCodeToInt :: ExitCode -> Int
    exitCodeToInt c = case c of
      ExitSuccess -> 0
      ExitFailure i -> i
    preludeSpan :: Span
    preludeSpan = FixtureSpan "prelude"

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

data CommandResult = CommandResult {exitCode :: ExitCode, stdout :: String, stderr :: String}

runSubprocess :: String -> String -> IO CommandResult
runSubprocess image cmd = do
  hPutStrLn IO.stderr $ T.pack $ "> " <> showCreateProcessForUser (shell cmd)
  (_, Just stdout, Just stderr, ps) <-
    createProcess (shell cmd) {std_out = CreatePipe, std_err = CreatePipe, delegate_ctlc = True}
  stdout' <- newEmptyMVar
  forkIO $ do
    stdout'' <- hPutAndGetContents ("[" <> image <> "(stdout)]") stdout
    putMVar stdout' stdout''
  stderr' <- newEmptyMVar
  forkIO $ do
    stderr'' <- hPutAndGetContents ("[" <> image <> "(stderr)]") stderr
    putMVar stderr' stderr''
  exitCode <- waitForProcess ps
  hClose stderr
  hClose stdout
  CommandResult exitCode <$> readMVar stdout' <*> readMVar stderr'

hPutAndGetContents :: String -> Handle -> IO String
hPutAndGetContents = hPutAndGetContents' ""
  where
    hPutAndGetContents' :: String -> String -> Handle -> IO String
    hPutAndGetContents' str console handle =
      do
        isClosed <- hIsClosed handle
        if isClosed
          then pure str
          else do
            isEof <- hIsEOF handle
            if isEof
              then pure str
              else do
                l <- hPutAndGetLine console handle
                hPutAndGetContents' (str <> l) console handle
    hPutAndGetLine :: String -> Handle -> IO String
    hPutAndGetLine console handle = do
      l <- hGetLine handle
      hPutStrLn IO.stderr $ T.pack console <> T.pack " " <> l
      pure $ T.unpack l

-- TODO: Implement by calling the Docker Engine API via Unix Socket.
-- This module should normally be implemented by making a request to the Docker Engine API via a Unix Socket.
-- However, it is not easy to implement HTTP communication over Unix Socket in Haskell.
-- The existing [docker-hs](https://hackage.haskell.org/package/docker) package is described
-- to configure Docker to communicate over TCP to avoid this problem.
-- The [http-client](https://hackage.haskell.org/package/http-client-0.7.17) package provides
-- a low-level API for HTTP but does not appear to support Unix Sockets.
runContainer :: String -> String -> IO CommandResult
runContainer image args = do
  -- TODO: Implement better default volume mounts and user-customisable methods.
  currentDir <- getCurrentDirectory
  let volume = currentDir <> ":/work"
  runSubprocess image $ "docker run --rm -v " <> volume <> " -w /work " <> image <> " " <> args
