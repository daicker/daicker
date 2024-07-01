{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE Strict #-}

module Language.Daicker.Executor where

import Control.Comonad.Cofree
import Data.Foldable (find)
import GHC.IO (unsafePerformIO)
import GHC.IO.Handle (hGetContents)
import Language.Daicker.AST
import Language.Daicker.Span (Span, mkSpan)
import qualified Language.Daicker.Span as S
import System.Exit (ExitCode (ExitFailure, ExitSuccess))
import System.Process

findDefine :: String -> Module Span -> Maybe (Define Span)
findDefine name (Module _ _ _ ds) = find (\(Define (Identifier n _) _ _) -> name == n) ds

execDefine :: Module Span -> Define Span -> Maybe (Expr ()) -> Either (String, Span) (Expr Span)
execDefine (Module _ _ _ ds) (Define _ e _) Nothing = eval [] e
execDefine (Module _ _ _ ds) (Define _ e _) (Just arg) = eval [] (S.span e :< EApp Nothing [e, switchAnn (\_ -> mkSpan "stdin" 1 1 1 2) arg])

switchAnn :: (a -> b) -> Expr a -> Expr b
switchAnn f e = case e of
  (ann :< ENull) -> f ann :< ENull
  (ann :< EBool v) -> f ann :< EBool v
  (ann :< ENumber v) -> f ann :< ENumber v
  (ann :< EString v) -> f ann :< EString v
  (ann :< EArray es) -> f ann :< EArray (map (switchAnn f) es)
  (ann :< EObject es) -> f ann :< EObject (map (\(Identifier i ann1, e) -> (Identifier i (f ann1), switchAnn f e)) es)
  (ann :< ERef (Identifier i ann1)) -> f ann :< ERef (Identifier i (f ann1))
  (ann :< EApp (Just (Identifier i ann1)) es) -> f ann :< EApp (Just $ Identifier i (f ann1)) (map (switchAnn f) es)
  (ann :< EApp Nothing es) -> f ann :< EApp Nothing (map (switchAnn f) es)
  (ann :< EFun is e) -> f ann :< EFun (map (\(Identifier i ann) -> Identifier i (f ann)) is) (switchAnn f e)

eval :: [(String, Expr Span)] -> Expr Span -> Either (String, Span) (Expr Span)
eval vars v = case v of
  s :< EArray vs -> (:<) s . EArray <$> mapM (eval vars) vs
  s :< EObject vs -> (:<) s . EObject <$> mapM (\(k, e) -> (,) k <$> eval vars e) vs
  s :< ERef (Identifier i _) -> case lookup i vars of
    Just a -> eval vars a
    Nothing -> Left ("not defined: " <> i, s)
  s :< EApp _ (a0@(_ :< ERef (Identifier i _)) : args) -> do
    args <- mapM (eval vars) args
    case lookup i stdLib of
      Just f -> Right $ f args
      Nothing -> case eval vars a0 of
        Right (s :< EFun params e) -> do
          let args' = zip (map (\(Identifier s _) -> s) params) args
          eval (vars <> args') e
        err -> err
  s :< EApp _ (a0 : args) -> do
    args <- mapM (eval vars) args
    case eval vars a0 of
      Right (s :< EFun params e) -> do
        let args' = zip (map (\(Identifier s _) -> s) params) args
        eval (vars <> args') e
      err -> err
  v -> Right v

stdLib :: [(String, [Expr Span] -> Expr Span)]
stdLib =
  [ ( "$",
      \strings -> do
        let cmds = map (\(_ :< EString s) -> s) strings
        let sp = foldl (\a b -> a S.<> S.span b) (S.span $ head strings) strings
        let (CommandResult i out err) = unsafePerformIO $ runSubprocess cmds
        sp
          :< EObject
            [ (Identifier "exitCode" sp, sp :< ENumber (fromIntegral $ exitCodeToInt i)),
              (Identifier "stdout" sp, sp :< EString out),
              (Identifier "stderr" sp, sp :< EString err)
            ]
    ),
    ( "$1",
      \strings -> do
        let cmds = map (\(_ :< EString s) -> s) strings
        let sp = foldl (\a b -> a S.<> S.span b) (S.span $ head strings) strings
        let (CommandResult _ out _) = unsafePerformIO $ runSubprocess cmds
        sp :< EString out
    ),
    ( "$2",
      \strings -> do
        let cmds = map (\(_ :< EString s) -> s) strings
        let sp = foldl (\a b -> a S.<> S.span b) (S.span $ head strings) strings
        let (CommandResult _ _ err) = unsafePerformIO $ runSubprocess cmds
        sp :< EString err
    ),
    ("+", \[s1 :< ENumber a, s2 :< ENumber b] -> (s1 S.<> s2) :< ENumber (a + b)),
    ("-", \[s1 :< ENumber a, s2 :< ENumber b] -> (s1 S.<> s2) :< ENumber (a - b)),
    ("*", \[s1 :< ENumber a, s2 :< ENumber b] -> (s1 S.<> s2) :< ENumber (a * b)),
    ("/", \[s1 :< ENumber a, s2 :< ENumber b] -> (s1 S.<> s2) :< ENumber (a / b))
  ]
  where
    exitCodeToInt :: ExitCode -> Int
    exitCodeToInt c = case c of
      ExitSuccess -> 0
      ExitFailure i -> i

data CommandResult = CommandResult {exitCode :: ExitCode, stdout :: String, stderr :: String}

runSubprocess :: [String] -> IO CommandResult
runSubprocess (cmd : args) = do
  (_, Just stdout, Just stderr, ps) <-
    createProcess (proc cmd args) {std_out = CreatePipe, std_err = CreatePipe}
  exitCode <- waitForProcess ps
  CommandResult exitCode <$> hGetContents stdout <*> hGetContents stderr
