{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE Strict #-}

module Language.Daicker.Executor where

import Control.Comonad.Cofree
import Control.Concurrent (forkIO)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, readMVar)
import Control.Monad (zipWithM)
import Data.Foldable (find)
import qualified Data.Text as T
import Data.Text.IO (hGetLine, hPutStrLn)
import Data.Tree (flatten)
import GHC.Base (join)
import GHC.IO (unsafePerformIO)
import GHC.IO.Handle (BufferMode (NoBuffering), Handle, hClose, hFlush, hGetChar, hGetContents, hIsClosed, hIsEOF)
import Language.Daicker.AST
import Language.Daicker.Error (CodeError (CodeError))
import Language.Daicker.Span (Span, mkSpan, union)
import qualified Language.Daicker.Span as S
import System.Directory (getCurrentDirectory)
import System.Exit (ExitCode (ExitFailure, ExitSuccess))
import System.IO (hSetBuffering)
import qualified System.IO as IO
import System.Process

findDefine :: String -> Module a -> Maybe (Define a)
findDefine name (_ :< Module _ _ _ ds) = find (\(_ :< Define (_ :< Identifier n) _ _) -> name == n) ds

execDefine :: Module Span -> Define Span -> Maybe (Expr ()) -> Either [CodeError] (Expr Span)
execDefine (_ :< Module _ _ _ ds) (_ :< Define _ e _) Nothing = eval [] e
execDefine (_ :< Module _ _ _ ds) (_ :< Define _ e _) (Just arg) = eval [] (S.span e :< EApp Nothing e (switchAnn (\_ -> mkSpan "stdin" 1 1 1 2) arg))

switchAnn :: (a -> b) -> Expr a -> Expr b
switchAnn f e = case e of
  (ann :< ENull) -> f ann :< ENull
  (ann :< EBool v) -> f ann :< EBool v
  (ann :< ENumber v) -> f ann :< ENumber v
  (ann :< EString v) -> f ann :< EString v
  (ann :< EArray es) -> f ann :< EArray (map (switchAnn f) es)
  (ann :< EObject es) -> f ann :< EObject (map (\(ann1 :< Identifier i, e) -> (f ann1 :< Identifier i, switchAnn f e)) es)

-- (ann :< ERef (Identifier i ann1)) -> f ann :< ERef (Identifier i (f ann1))
-- (ann :< EApp (Just (Identifier i ann1)) a b) -> f ann :< EApp (Just $ Identifier i (f ann1)) (switchAnn f a) (switchAnn f b)
-- (ann :< EApp Nothing a b) -> f ann :< EApp Nothing (switchAnn f a) (switchAnn f b)
-- (ann :< EFun (Just (Identifier i a)) e) -> f ann :< EFun (Just (Identifier i (f a))) (switchAnn f e)
-- (ann :< EFun Nothing e) -> f ann :< EFun Nothing (switchAnn f e)

eval :: [(String, Expr Span)] -> Expr Span -> Either [CodeError] (Expr Span)
eval vars v = case v of
  s :< EArray vs -> (:<) s . EArray <$> mapM (eval vars) vs
  s :< EObject vs -> (:<) s . EObject <$> mapM (\(k, e) -> (,) k <$> eval vars e) vs
  s :< ERef (_ :< Identifier i) -> case lookup i vars of
    Just a -> eval vars a
    Nothing -> Left [CodeError ("not defined: " <> i) s]
  s :< EApp image a0@(_ :< ERef (_ :< Identifier i)) arg -> do
    arg <- eval vars arg
    case lookup i stdLib of
      Just f -> Right $ f image arg
      Nothing -> case eval vars a0 of
        Right (s :< EFun (Just pm) e) -> do
          args <- patternMatch pm arg
          eval (vars <> args) e
        err -> err
  s :< EApp _ f arg -> do
    arg <- eval vars arg
    case eval vars f of
      Right (s :< EFun (Just pm) e) -> do
        args <- patternMatch pm arg
        eval (vars <> args) e
      err -> err
  s :< EProperty e (_ :< Identifier i1) -> do
    case eval vars e of
      Right (_ :< EObject vs) -> case find (\(s :< Identifier i2, _) -> i1 == i2) vs of
        Just (_, v) -> pure v
        Nothing -> pure $ s :< ENull
      Right (s :< _) -> Left [CodeError "Accessors can only be used on objects" s]
  v -> Right v

patternMatch :: PatternMatchAssign Span -> Expr Span -> Either [CodeError] [(String, Expr Span)]
patternMatch pma e = case pma of
  _ :< PMAAnyValue (_ :< Identifier i) -> pure [(i, e)]
  _ :< PMAArray as -> case e of
    (_ :< EArray vs) -> concat <$> zipWithM patternMatch as vs
    (s :< _) -> Left [CodeError "pattern match: unexpected type" s]
  _ :< PMAObject as -> case e of
    (s :< EObject es) -> concat <$> mapM (uncurry $ objectMatch es) as
    (s :< _) -> Left [CodeError "pattern match: unexpected type" s]
    where
      objectMatch es (s :< Identifier i1) a = case find (\(s :< Identifier i2, _) -> i1 == i2) es of
        Nothing -> Left [CodeError ("not found key: " <> i1) s]
        Just (_, e) -> patternMatch a e

stdLib :: [(String, Maybe (EImage Span) -> Expr Span -> Expr Span)]
stdLib =
  [ ( "$",
      \image (_ :< EArray strings) -> do
        let cmds = map (\(_ :< EString s) -> s) strings
        let sp = foldl (\a b -> a `union` S.span b) (S.span $ head strings) strings
        let (CommandResult i out err) = case image of
              Nothing -> unsafePerformIO $ runSubprocess "local" cmds
              Just (_ :< Identifier image) -> unsafePerformIO $ runContainer image cmds
        sp
          :< EObject
            [ (sp :< Identifier "exitCode", sp :< ENumber (fromIntegral $ exitCodeToInt i)),
              (sp :< Identifier "stdout", sp :< EString out),
              (sp :< Identifier "stderr", sp :< EString err)
            ]
    ),
    ( "$1",
      \image (_ :< EArray strings) -> do
        let cmds = map (\(_ :< EString s) -> s) strings
        let sp = foldl (\a b -> a `union` S.span b) (S.span $ head strings) strings
        let (CommandResult _ out _) = case image of
              Nothing -> unsafePerformIO $ runSubprocess "local" cmds
              Just (_ :< Identifier image) -> unsafePerformIO $ runContainer image cmds
        sp :< EString out
    ),
    ( "$2",
      \image (_ :< EArray strings) -> do
        let cmds = map (\(_ :< EString s) -> s) strings
        let sp = foldl (\a b -> a `union` S.span b) (S.span $ head strings) strings
        let (CommandResult _ _ err) = case image of
              Nothing -> unsafePerformIO $ runSubprocess "local" cmds
              Just (_ :< Identifier image) -> unsafePerformIO $ runContainer image cmds
        sp :< EString err
    ),
    ( ";",
      \_ (s :< EArray [e1, e2]) -> unsafePerformIO $ do
        _ <- pure e1 -- execute forcibly
        pure e2
    ),
    ( "|>",
      \_ (s :< EArray [e1, e2]) -> s :< EApp Nothing e1 e2
    ),
    ("+", \_ (_ :< EArray [s1 :< ENumber a, s2 :< ENumber b]) -> (s1 `union` s2) :< ENumber (a + b)),
    ("-", \_ (_ :< EArray [s1 :< ENumber a, s2 :< ENumber b]) -> (s1 `union` s2) :< ENumber (a - b)),
    ("*", \_ (_ :< EArray [s1 :< ENumber a, s2 :< ENumber b]) -> (s1 `union` s2) :< ENumber (a * b)),
    ("/", \_ (_ :< EArray [s1 :< ENumber a, s2 :< ENumber b]) -> (s1 `union` s2) :< ENumber (a / b))
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
