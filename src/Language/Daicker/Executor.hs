{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE Strict #-}

module Language.Daicker.Executor where

import Control.Comonad.Cofree
import Control.Concurrent (forkIO)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, readMVar)
import Control.Monad (zipWithM)
import Control.Monad.Except (ExceptT, MonadError (throwError))
import Control.Monad.IO.Class
import qualified Control.Monad.IO.Class (liftIO)
import Data.Foldable (find)
import Data.List (intercalate)
import qualified Data.Text as T
import Data.Text.IO (hGetLine, hPutStrLn)
import Data.Tree (flatten)
import Debug.Trace (traceShow)
import GHC.Base (join)
import GHC.IO.Handle (Handle, hClose, hFlush, hGetChar, hGetContents, hIsClosed, hIsEOF)
import Language.Daicker.AST
import Language.Daicker.Error (RuntimeError (RuntimeError))
import Language.Daicker.Span (Span, mkSpan, union)
import qualified Language.Daicker.Span as S
import System.Directory (getCurrentDirectory)
import System.Exit (ExitCode (ExitFailure, ExitSuccess), exitFailure)
import System.IO (hSetBuffering)
import qualified System.IO as IO
import System.Process

findDefine :: String -> [Statement a] -> Maybe (Statement a)
findDefine n = find (\s -> isDefine s && name s == n)
  where
    isDefine (_ :< SDefine {}) = True
    isDefine _ = False
    name (_ :< SDefine (_ :< Identifier n) _ _) = n

findType :: String -> [Statement a] -> Maybe (Statement a)
findType n = find (\s -> isType s && name s == n)
  where
    isType (_ :< STypeDefine {}) = True
    isType _ = False
    name (_ :< STypeDefine (_ :< Identifier n) _) = n

execDefine :: Module Span -> Expr Span -> [(Expr Span, Expansion)] -> ExceptT RuntimeError IO (Expr Span)
execDefine (_ :< Module {}) e args = eval [] (S.span e :< EApp Nothing e args)

eval :: [(String, Expr Span)] -> Expr Span -> ExceptT RuntimeError IO (Expr Span)
eval vars v = case v of
  s :< EArray vs -> (:<) s . EArray <$> mapM (eval vars) vs
  s :< EObject vs -> (:<) s . EObject <$> mapM (\(k, e) -> (,) k <$> eval vars e) vs
  s :< ERef (_ :< Identifier i) -> case lookup i vars of
    Just a -> eval vars a
    Nothing -> throwError $ RuntimeError ("not defined: " <> i) s (ExitFailure 1)
  s :< EApp image a0@(_ :< ERef (_ :< Identifier i)) args -> do
    args <-
      mapM
        ( \(e, expansion) ->
            if expansion
              then expand <$> eval vars e
              else (: []) <$> eval vars e
        )
        args
    case lookup i stdLib of
      Just f -> f image (join args)
      Nothing -> do
        (s :< EFun pms e ex) <- eval vars a0
        args <- patternMatch ex pms (join args)
        eval (vars <> args) e
  s :< EApp _ f args -> do
    args <-
      mapM
        ( \(e, expansion) ->
            if expansion
              then expand <$> eval vars e
              else (: []) <$> eval vars e
        )
        args
    (s :< EFun pms e ex) <- eval vars f
    args <- patternMatch ex pms (join args)
    eval (vars <> args) e
  s :< EProperty e (_ :< Identifier i1) -> do
    e <- eval vars e
    case e of
      (_ :< EObject vs) -> case find (\(s :< Identifier i2, _) -> i1 == i2) vs of
        Just (_, v) -> pure v
        Nothing -> pure $ s :< ENull
      (s :< _) -> throwError $ RuntimeError "Accessors can only be used on objects" s (ExitFailure 1)
  v -> pure v
  where
    expand (_ :< EArray es) = es

patternMatch :: Bool -> [PatternMatchAssign Span] -> [Expr Span] -> ExceptT RuntimeError IO [(String, Expr Span)]
patternMatch True [_ :< PMAAnyValue (_ :< Identifier i)] es = pure [(i, S.span (head es) `union` S.span (last es) :< EArray es)]
patternMatch ex (pma : pmas) (e : es) = (<>) <$> patternMatchOne pma e <*> patternMatch ex pmas es
patternMatch _ [] [] = pure []

patternMatchOne :: PatternMatchAssign Span -> Expr Span -> ExceptT RuntimeError IO [(String, Expr Span)]
patternMatchOne pma e = case pma of
  _ :< PMAAnyValue (_ :< Identifier i) -> pure [(i, e)]
  _ :< PMAArray as -> case e of
    (_ :< EArray vs) -> concat <$> zipWithM patternMatchOne as vs
    (s :< _) -> throwError $ RuntimeError "pattern match: unexpected type" s (ExitFailure 1)
  _ :< PMAObject as -> case e of
    (s :< EObject es) -> concat <$> mapM (uncurry $ objectMatch es) as
    (s :< _) -> throwError $ RuntimeError "pattern match: unexpected type" s (ExitFailure 1)
    where
      objectMatch es (s :< Identifier i1) a = case find (\(s :< Identifier i2, _) -> i1 == i2) es of
        Nothing -> throwError $ RuntimeError ("not found key: " <> i1) s (ExitFailure 1)
        Just (_, e) -> patternMatchOne a e

stdLib :: [(String, Maybe (EImage Span) -> [Expr Span] -> ExceptT RuntimeError IO (Expr Span))]
stdLib =
  [ ( "$_",
      \image strings -> do
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
    ),
    ( "$",
      \image strings -> do
        let cmds = map (\(_ :< EString s) -> s) strings
        let sp = foldl (\a b -> a `union` S.span b) (S.span $ head strings) strings
        (CommandResult i out _) <- liftIO $ case image of
          Nothing -> runSubprocess "local" cmds
          Just (_ :< Identifier image) -> runContainer image cmds
        case i of
          ExitSuccess -> pure $ sp :< EString out
          ExitFailure _ -> throwError $ RuntimeError ("Error command exec: " <> "$ " <> unwords cmds) sp i
    ),
    ( "$1",
      \image strings -> do
        let cmds = map (\(_ :< EString s) -> s) strings
        let sp = foldl (\a b -> a `union` S.span b) (S.span $ head strings) strings
        (CommandResult i out _) <- liftIO $ case image of
          Nothing -> runSubprocess "local" cmds
          Just (_ :< Identifier image) -> runContainer image cmds
        case i of
          ExitSuccess -> pure $ sp :< EString out
          ExitFailure _ -> throwError $ RuntimeError ("Error command exec: " <> "$1 " <> unwords cmds) sp i
    ),
    ( "$2",
      \image strings -> do
        let cmds = map (\(_ :< EString s) -> s) strings
        let sp = foldl (\a b -> a `union` S.span b) (S.span $ head strings) strings
        (CommandResult i _ err) <- liftIO $ case image of
          Nothing -> runSubprocess "local" cmds
          Just (_ :< Identifier image) -> runContainer image cmds
        case i of
          ExitSuccess -> pure $ sp :< EString err
          ExitFailure _ -> throwError $ RuntimeError ("Error command exec: " <> "$1 " <> unwords cmds) sp i
    ),
    ( ";",
      \_ [e1, e2] -> do
        _ <- pure e1 -- execute forcibly
        pure e2
    ),
    ( "|>",
      \_ [e1@(s1 :< _), e2@(s2 :< _)] -> pure $ s1 `S.union` s2 :< EApp Nothing e1 [(e2, False)]
    ),
    ("+", \_ [s1 :< ENumber a, s2 :< ENumber b] -> pure $ (s1 `union` s2) :< ENumber (a + b)),
    ("-", \_ [s1 :< ENumber a, s2 :< ENumber b] -> pure $ (s1 `union` s2) :< ENumber (a - b)),
    ("*", \_ [s1 :< ENumber a, s2 :< ENumber b] -> pure $ (s1 `union` s2) :< ENumber (a * b)),
    ("/", \_ [s1 :< ENumber a, s2 :< ENumber b] -> pure $ (s1 `union` s2) :< ENumber (a / b))
  ]
  where
    exitCodeToInt :: ExitCode -> Int
    exitCodeToInt c = case c of
      ExitSuccess -> 0
      ExitFailure i -> i

data CommandResult = CommandResult {exitCode :: ExitCode, stdout :: String, stderr :: String}

runSubprocess :: String -> [String] -> IO CommandResult
runSubprocess image (cmd : args) = do
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
