{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE Strict #-}

module Language.Daicker.Executor where

import Data.Foldable (find)
import GHC.IO (unsafePerformIO)
import GHC.IO.Handle (hGetContents)
import Language.Daicker.AST
import Language.Daicker.Span (Span)
import qualified Language.Daicker.Span as S
import System.Exit (ExitCode (ExitFailure, ExitSuccess))
import System.Process

class Evaluatable a where
  eval :: [(String, Expr)] -> a -> Either (String, Span) Expr

findDefine :: String -> Module -> Maybe Define
findDefine name (Module _ _ _ ds) = find (\(Define (Identifier n _) _ _) -> name == n) ds

execDefine :: Module -> Define -> Either (String, Span) Expr
execDefine (Module _ _ _ ds) (Define _ e _) = eval vars e
  where
    vars = map (\(Define (Identifier name _) e _) -> (name, e)) ds

instance Evaluatable Expr where
  eval :: [(String, Expr)] -> Expr -> Either (String, Span) Expr
  eval vars v = case v of
    EArray vs s -> EArray <$> mapM (eval vars) vs <*> pure s
    EObject vs s -> EObject <$> mapM (\(k, e) -> (,) k <$> eval vars e) vs <*> pure s
    ERef (Identifier i _) s -> case lookup i vars of
      Just a -> eval vars a
      Nothing -> Left ("not defined: " <> i, s)
    EApp _ (a0@(ERef (Identifier i _) _) : args) s -> case lookup i stdLib of
      Just f -> Right $ f args
      Nothing -> case eval vars a0 of
        Right (EFun params e s) -> do
          let args' = zip (map (\(Identifier s _) -> s) params) args
          eval (vars <> args') e
        err -> err
    EApp _ (a0 : args) s -> case eval vars a0 of
      Right (EFun params e s) -> do
        let args' = zip (map (\(Identifier s _) -> s) params) args
        eval (vars <> args') e
      err -> err
    v -> Right v

stdLib :: [(String, [Expr] -> Expr)]
stdLib =
  [ ( "$",
      \strings -> do
        let cmds = map (\(EString s sp) -> s) strings
        let sp = foldl (\a b -> a S.<> S.span b) (S.span $ head strings) strings
        let (CommandResult i out err) = unsafePerformIO $ runSubprocess cmds
        EObject
          [ (Identifier "exitCode" sp, ENumber (fromIntegral $ exitCodeToInt i) sp),
            (Identifier "stdout" sp, EString out sp),
            (Identifier "stderr" sp, EString err sp)
          ]
          sp
    ),
    ( "$1",
      \strings -> do
        let cmds = map (\(EString s sp) -> s) strings
        let sp = foldl (\a b -> a S.<> S.span b) (S.span $ head strings) strings
        let (CommandResult _ out _) = unsafePerformIO $ runSubprocess cmds
        EString out sp
    ),
    ( "$2",
      \strings -> do
        let cmds = map (\(EString s sp) -> s) strings
        let sp = foldl (\a b -> a S.<> S.span b) (S.span $ head strings) strings
        let (CommandResult _ _ err) = unsafePerformIO $ runSubprocess cmds
        EString err sp
    ),
    ("+", \[ENumber a s1, ENumber b s2] -> ENumber (a + b) (s1 S.<> s2)),
    ("-", \[ENumber a s1, ENumber b s2] -> ENumber (a - b) (s1 S.<> s2)),
    ("*", \[ENumber a s1, ENumber b s2] -> ENumber (a * b) (s1 S.<> s2)),
    ("/", \[ENumber a s1, ENumber b s2] -> ENumber (a / b) (s1 S.<> s2))
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
