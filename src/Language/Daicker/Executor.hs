{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE Strict #-}

module Language.Daicker.Executor where

import Data.Foldable (find)
import GHC.IO (unsafePerformIO)
import GHC.IO.Handle (hGetContents)
import Language.Daicker.AST
import Language.Daicker.Span (Span)
import System.Exit (ExitCode (ExitFailure, ExitSuccess))
import System.Process

class Evaluatable a where
  eval :: [(String, Expr)] -> a -> Either (String, Span) Expr

findDefine :: String -> Module -> Maybe Define
findDefine name (Module _ _ _ ds) = find (\(Define (Identifier n _) _ _) -> name == n) ds

execDefine :: Define -> Either (String, Span) Expr
execDefine (Define _ e _) = eval [] e

instance Evaluatable Expr where
  eval :: [(String, Expr)] -> Expr -> Either (String, Span) Expr
  eval vars v = case v of
    EArray vs s -> EArray <$> mapM (eval vars) vs <*> pure s
    EObject vs s -> EObject <$> mapM (\(k, e) -> (,) k <$> eval vars e) vs <*> pure s
    ERef (Identifier i _) s -> case lookup i vars of
      Just a -> Right a
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
      \[EString s sp] -> do
        let (CommandResult i out err) = unsafePerformIO $ runSubprocess s
        EObject
          [ (Identifier "exitCode" sp, ENumber (fromIntegral $ exitCodeToInt i) sp),
            (Identifier "stdout" sp, EString out sp),
            (Identifier "stderr" sp, EString err sp)
          ]
          sp
    ),
    ("$1", \[EString s sp] -> unsafePerformIO $ EString . stdout <$> runSubprocess s <*> pure sp),
    ("$2", \[EString s sp] -> unsafePerformIO $ EString . stderr <$> runSubprocess s <*> pure sp)
  ]
  where
    exitCodeToInt :: ExitCode -> Int
    exitCodeToInt c = case c of
      ExitSuccess -> 0
      ExitFailure i -> i

data CommandResult = CommandResult {exitCode :: ExitCode, stdout :: String, stderr :: String}

runSubprocess :: String -> IO CommandResult
runSubprocess cmd = do
  (_, Just stderr, Just stdout, ps) <-
    createProcess (proc cmd []) {std_out = CreatePipe, std_err = CreatePipe}
  exitCode <- waitForProcess ps
  CommandResult exitCode <$> hGetContents stdout <*> hGetContents stderr
