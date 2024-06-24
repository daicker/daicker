{-# LANGUAGE InstanceSigs #-}

module Language.Daicker.Executor where

import Data.Foldable (find)
import GHC.IO (unsafePerformIO)
import Language.Daicker.AST
import Language.Daicker.Span (Span)
import System.Process (callCommand)

class Evaluatable a where
  eval :: [(String, Expr)] -> a -> Either (String, Span) Expr

findDefine :: String -> Module -> Maybe Define
findDefine name (Module _ _ _ ds) = find (\(Define (Identifier n _) _ _) -> name == n) ds

execDefine :: Define -> IO (Either (String, Span) Expr)
execDefine (Define _ e _) = pure $ eval [] e

instance Evaluatable Expr where
  eval :: [(String, Expr)] -> Expr -> Either (String, Span) Expr
  eval vars v = case v of
    EArray vs s -> EArray <$> mapM (eval vars) vs <*> pure s
    EObject vs s -> EObject <$> mapM (\(k, e) -> (,) k <$> eval vars e) vs <*> pure s
    ERef (Identifier i _) s -> case lookup i vars of
      Just a -> Right a
      Nothing -> Left ("not defined: " <> i, s)
    EApp _ (a0@(ERef (Identifier i _) _) : args) s -> case lookup i stdLib of
      Just f -> Right $ unsafePerformIO $ f args
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

stdLib :: [(String, [Expr] -> IO Expr)]
stdLib =
  [ ("$", \[EString s sp] -> ENull sp <$ callCommand s)
  ]
