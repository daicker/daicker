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
import qualified Data.Text.IO as T
import Data.Tree (flatten)
import Debug.Trace (traceShow)
import GHC.Base (join)
import GHC.IO.Handle (Handle, hClose, hFlush, hGetChar, hGetContents, hIsClosed, hIsEOF)
import Language.Daicker.AST
import Language.Daicker.Bundler (ExprBundle (ExprBundle), lookupExpr, toExprBundle)
import Language.Daicker.Error (RuntimeError (RuntimeError))
import Language.Daicker.Span (Span (FixtureSpan), mkSpan, union)
import qualified Language.Daicker.Span as S
import System.Directory (getCurrentDirectory)
import System.Exit (ExitCode (ExitFailure, ExitSuccess), exitFailure)
import System.IO (hSetBuffering)
import qualified System.IO as IO
import System.Process

execDefine :: Expr Span -> [(Expr Span, Expansion)] -> [ExprBundle] -> ExceptT RuntimeError IO (Expr Span)
execDefine e args bundles = eval bundles (S.span e :< EApp Nothing e args)

eval :: [ExprBundle] -> Expr Span -> ExceptT RuntimeError IO (Expr Span)
eval bundles v = case v of
  e@(s :< EError {}) -> pure e
  s :< EArray vs -> (:<) s . EArray <$> mapM (eval bundles) vs
  s :< EObject vs -> (:<) s . EObject <$> mapM (\(k, e) -> (,) k <$> eval bundles e) vs
  s :< ERef (_ :< Identifier i) -> case lookupExpr i bundles of
    Just a -> eval bundles a
    Nothing -> throwError $ RuntimeError ("not defined: " <> i) s (ExitFailure 1)
  s :< EApp image f args -> do
    args <-
      mapM
        ( \(e, expansion) ->
            if expansion
              then expand <$> eval bundles e
              else (: []) <$> eval bundles e
        )
        args
    f' <- eval bundles f
    case f' of
      (s :< EFun pms e ex) -> do
        args <- patternMatch ex pms (join args)
        eval (map toExprBundle args <> bundles) e
      (s :< EFixtureFun pms e ex) -> do
        liftIO $ e image (join args)
      (s :< e) ->
        case args of
          [] -> pure $ s :< e
          _ -> throwError $ RuntimeError "Not a function" s (ExitFailure 1)
  s :< EProperty e (_ :< Identifier i1) -> do
    e <- eval bundles e
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
