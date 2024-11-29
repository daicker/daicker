{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE Strict #-}

module Language.Daicker.Executor where

import Control.Comonad.Cofree
import Control.Concurrent (forkIO)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, readMVar)
import Control.Monad (zipWithM)
import Control.Monad.Except (ExceptT, MonadError (throwError), liftEither)
import Control.Monad.IO.Class
import qualified Control.Monad.IO.Class (liftIO)
import Data.Foldable (find)
import Data.List (intercalate)
import Data.Maybe (fromJust, fromMaybe)
import qualified Data.Text as T
import Data.Text.IO (hGetLine, hPutStrLn)
import qualified Data.Text.IO as T
import Data.Tree (flatten)
import Debug.Trace (traceShow)
import GHC.Base (join)
import GHC.IO.Handle (Handle, hClose, hFlush, hGetChar, hGetContents, hIsClosed, hIsEOF)
import Language.Daicker.AST
import Language.Daicker.Bundler (Bundle (Bundle), ModuleBundle (ModuleBundle, currentModule, dependencyModules, preludeModule), abandonType, appendArgument, findExpr, packArg)
import Language.Daicker.Error (RuntimeError (RuntimeError), StaticError (StaticError))
import Language.Daicker.Span (Span (FixtureSpan), mkSpan, union)
import qualified Language.Daicker.Span as S
import System.Directory (getCurrentDirectory)
import System.Exit (ExitCode (ExitFailure, ExitSuccess), exitFailure)
import System.IO (hSetBuffering)
import qualified System.IO as IO
import System.Process

eval :: (Eq a) => Bundle a -> Expr a -> ExceptT (RuntimeError a) IO (Expr a)
eval bundle@(Bundle ms as) v = case v of
  e@(s :< EError {}) -> pure e
  s :< EArray vs -> (:<) s . EArray <$> mapM (eval bundle) vs
  s :< EObject vs -> (:<) s . EObject <$> mapM (\(k, e) -> (,) k <$> eval bundle e) vs
  s :< EVar i@(_ :< Identifier name) -> case findExpr bundle name of
    Just (bundle, a) -> eval bundle a
    Nothing -> throwError $ RuntimeError ("not defined: " <> name) s (ExitFailure 1)
  s :< ECall f args -> do
    args <-
      mapM
        ( \case
            (s :< PositionedArgument False e) -> (s :<) . PositionedArgument False <$> eval bundle e
            (s :< KeywordArgument i e) -> (s :<) . KeywordArgument i <$> eval bundle e
        )
        args
    f' <- eval bundle f
    case f' of
      (s :< ELambda pms (s' :< e) t) -> do
        args <- packArg pms args
        eval (Bundle ms (appendArgument as args)) (s' :< e)
      (s :< EFixtureFun pms e ex) -> do
        args <- packArg pms args
        res <- liftIO $ e s (map abandonType args)
        eval bundle res
      (s :< e) ->
        case args of
          [] -> pure $ s :< e
          _ -> throwError $ RuntimeError "Not a function" s (ExitFailure 1)
  s :< EAccessor e (_ :< key) -> do
    e <- eval bundle e
    case e of
      (_ :< EObject vs) -> case find (\(_ :< key', _) -> key == key') vs of -- TODO: Implement equality
        Just (_, v) -> pure v
        Nothing -> pure $ s :< ENull
      (s :< _) -> throwError $ RuntimeError "Accessors can only be used on objects" s (ExitFailure 1)
  v -> pure v
  where
    expand (_ :< EArray es) = es
