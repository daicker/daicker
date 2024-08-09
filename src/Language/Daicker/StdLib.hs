module Language.Daicker.StdLib where

import Control.Comonad.Cofree (Cofree ((:<)))
import Control.Concurrent (putMVar, readMVar)
import Control.Concurrent.MVar (newEmptyMVar)
import Control.Monad.IO.Class (liftIO)
import Data.List (intercalate)
import qualified Data.Text as T
import Data.Text.IO (hGetLine, hPutStrLn)
import GHC.Conc (forkIO)
import GHC.IO.Exception (ExitCode (ExitFailure))
import GHC.IO.Handle (Handle)
import Language.Daicker.AST (Define' (Define), Expr, Expr' (..), Identifier' (Identifier), Module, Module' (Module), PatternMatchAssign' (PMAAnyValue), Statement' (SDefine))
import Language.Daicker.Span (Span (FixtureSpan), union)
import qualified Language.Daicker.Span as S
import System.Directory (getCurrentDirectory)
import System.Directory.Internal.Prelude (hClose)
import System.Exit (ExitCode (ExitSuccess))
import System.IO (hIsClosed, hIsEOF)
import qualified System.IO as IO
import System.Process (CreateProcess (..), createProcess, proc, waitForProcess)
import System.Process.Internals (StdStream (CreatePipe))

prelude :: Module Span
prelude =
  preludeSpan
    :< Module
      []
      Nothing
      [ preludeSpan
          :< SDefine
            ( preludeSpan
                :< Define
                  (preludeSpan :< Identifier "$_")
                  ( preludeSpan
                      :< EFixtureFun
                        [preludeSpan :< PMAAnyValue (preludeSpan :< Identifier "args")]
                        ( \image strings -> do
                            let cmds = map (\(_ :< EString s) -> s) strings
                            let sp = foldl (\a b -> a `union` S.span b) (S.span $ head strings) strings
                            (CommandResult i out err) <- liftIO $ case image of
                              Nothing -> runSubprocess "local" cmds
                              Just (_ :< Identifier image) -> runContainer image cmds
                            pure $
                              sp
                                :< EObject
                                  [ (sp :< Identifier "exitCode", sp :< ENumber (fromIntegral $ exitCodeToInt i)),
                                    (sp :< Identifier "stdout", sp :< EString out),
                                    (sp :< Identifier "stderr", sp :< EString err)
                                  ]
                        )
                        True
                  )
                  Nothing
            ),
        preludeSpan
          :< SDefine
            ( preludeSpan
                :< Define
                  (preludeSpan :< Identifier "$")
                  ( preludeSpan
                      :< EFixtureFun
                        [preludeSpan :< PMAAnyValue (preludeSpan :< Identifier "args")]
                        ( \image strings -> do
                            let cmds = map (\(_ :< EString s) -> s) strings
                            let sp = foldl (\a b -> a `union` S.span b) (S.span $ head strings) strings
                            (CommandResult i out _) <- liftIO $ case image of
                              Nothing -> runSubprocess "local" cmds
                              Just (_ :< Identifier image) -> runContainer image cmds
                            case i of
                              ExitSuccess -> pure $ sp :< EString out
                              ExitFailure _ -> pure $ sp :< EError ("Error command exec: " <> "$ " <> unwords cmds) i
                        )
                        True
                  )
                  Nothing
            ),
        preludeSpan
          :< SDefine
            ( preludeSpan
                :< Define
                  (preludeSpan :< Identifier "$1")
                  ( preludeSpan
                      :< EFixtureFun
                        [preludeSpan :< PMAAnyValue (preludeSpan :< Identifier "args")]
                        ( \image strings -> do
                            let cmds = map (\(_ :< EString s) -> s) strings
                            let sp = foldl (\a b -> a `union` S.span b) (S.span $ head strings) strings
                            (CommandResult i out _) <- liftIO $ case image of
                              Nothing -> runSubprocess "local" cmds
                              Just (_ :< Identifier image) -> runContainer image cmds
                            case i of
                              ExitSuccess -> pure $ sp :< EString out
                              ExitFailure _ -> pure $ sp :< EError ("Error command exec: " <> "$1 " <> unwords cmds) i
                        )
                        True
                  )
                  Nothing
            ),
        preludeSpan
          :< SDefine
            ( preludeSpan
                :< Define
                  (preludeSpan :< Identifier "$2")
                  ( preludeSpan
                      :< EFixtureFun
                        [preludeSpan :< PMAAnyValue (preludeSpan :< Identifier "args")]
                        ( \image strings -> do
                            let cmds = map (\(_ :< EString s) -> s) strings
                            let sp = foldl (\a b -> a `union` S.span b) (S.span $ head strings) strings
                            (CommandResult i _ err) <- liftIO $ case image of
                              Nothing -> runSubprocess "local" cmds
                              Just (_ :< Identifier image) -> runContainer image cmds
                            case i of
                              ExitSuccess -> pure $ sp :< EString err
                              ExitFailure _ -> pure $ sp :< EError ("Error command exec: " <> "$1 " <> unwords cmds) i
                        )
                        True
                  )
                  Nothing
            ),
        preludeSpan
          :< SDefine
            ( preludeSpan
                :< Define
                  (preludeSpan :< Identifier ";")
                  ( preludeSpan
                      :< EFixtureFun
                        [ preludeSpan :< PMAAnyValue (preludeSpan :< Identifier "a"),
                          preludeSpan :< PMAAnyValue (preludeSpan :< Identifier "b")
                        ]
                        ( \_ [e1, e2] -> do
                            _ <- pure e1 -- execute forcibly
                            pure e2
                        )
                        False
                  )
                  Nothing
            ),
        preludeSpan
          :< SDefine
            ( preludeSpan
                :< Define
                  (preludeSpan :< Identifier "|>")
                  ( preludeSpan
                      :< EFixtureFun
                        [ preludeSpan :< PMAAnyValue (preludeSpan :< Identifier "a"),
                          preludeSpan :< PMAAnyValue (preludeSpan :< Identifier "b")
                        ]
                        ( \_ [e1@(s1 :< _), e2@(s2 :< _)] ->
                            pure $
                              s1 `S.union` s2
                                :< EApp
                                  Nothing
                                  e1
                                  [(e2, False)]
                        )
                        False
                  )
                  Nothing
            ),
        preludeSpan
          :< SDefine
            ( preludeSpan
                :< Define
                  (preludeSpan :< Identifier "+")
                  ( preludeSpan
                      :< EFixtureFun
                        [ preludeSpan :< PMAAnyValue (preludeSpan :< Identifier "a"),
                          preludeSpan :< PMAAnyValue (preludeSpan :< Identifier "b")
                        ]
                        (\_ [s1 :< ENumber a, s2 :< ENumber b] -> pure $ (s1 `union` s2) :< ENumber (a + b))
                        False
                  )
                  Nothing
            ),
        preludeSpan
          :< SDefine
            ( preludeSpan
                :< Define
                  (preludeSpan :< Identifier "+")
                  ( preludeSpan
                      :< EFixtureFun
                        [ preludeSpan :< PMAAnyValue (preludeSpan :< Identifier "a"),
                          preludeSpan :< PMAAnyValue (preludeSpan :< Identifier "b")
                        ]
                        (\_ [s1 :< ENumber a, s2 :< ENumber b] -> pure $ (s1 `union` s2) :< ENumber (a + b))
                        False
                  )
                  Nothing
            ),
        preludeSpan
          :< SDefine
            ( preludeSpan
                :< Define
                  (preludeSpan :< Identifier "-")
                  ( preludeSpan
                      :< EFixtureFun
                        [ preludeSpan :< PMAAnyValue (preludeSpan :< Identifier "a"),
                          preludeSpan :< PMAAnyValue (preludeSpan :< Identifier "b")
                        ]
                        (\_ [s1 :< ENumber a, s2 :< ENumber b] -> pure $ (s1 `union` s2) :< ENumber (a - b))
                        False
                  )
                  Nothing
            ),
        preludeSpan
          :< SDefine
            ( preludeSpan
                :< Define
                  (preludeSpan :< Identifier "*")
                  ( preludeSpan
                      :< EFixtureFun
                        [ preludeSpan :< PMAAnyValue (preludeSpan :< Identifier "a"),
                          preludeSpan :< PMAAnyValue (preludeSpan :< Identifier "b")
                        ]
                        (\_ [s1 :< ENumber a, s2 :< ENumber b] -> pure $ (s1 `union` s2) :< ENumber (a * b))
                        False
                  )
                  Nothing
            ),
        preludeSpan
          :< SDefine
            ( preludeSpan
                :< Define
                  (preludeSpan :< Identifier "/")
                  ( preludeSpan
                      :< EFixtureFun
                        [ preludeSpan :< PMAAnyValue (preludeSpan :< Identifier "a"),
                          preludeSpan :< PMAAnyValue (preludeSpan :< Identifier "b")
                        ]
                        (\_ [s1 :< ENumber a, s2 :< ENumber b] -> pure $ (s1 `union` s2) :< ENumber (a / b))
                        False
                  )
                  Nothing
            )
      ]
  where
    exitCodeToInt :: ExitCode -> Int
    exitCodeToInt c = case c of
      ExitSuccess -> 0
      ExitFailure i -> i
    preludeSpan :: Span
    preludeSpan = FixtureSpan "prelude"

data CommandResult = CommandResult {exitCode :: ExitCode, stdout :: String, stderr :: String}

runSubprocess :: String -> [String] -> IO CommandResult
runSubprocess image (cmd : args) = do
  hPutStrLn IO.stderr $ T.pack $ "> " <> unwords (cmd : args)
  (_, Just stdout, Just stderr, ps) <-
    createProcess (proc cmd args) {std_out = CreatePipe, std_err = CreatePipe, delegate_ctlc = True}
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
runContainer :: String -> [String] -> IO CommandResult
runContainer image args = do
  -- TODO: Implement better default volume mounts and user-customisable methods.
  currentDir <- getCurrentDirectory
  let volume = currentDir <> ":/work"
  runSubprocess image $
    ["docker", "run", "--rm", "-v", volume, "-w", "/work", image] <> args
