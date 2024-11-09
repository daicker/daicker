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
import Language.Daicker.Bundler (Bundle (Bundle, current, modules, prelude, statements), ModuleBundle, findExpr, findExprWithBundle)
import Language.Daicker.Error (RuntimeError (RuntimeError), StaticError (StaticError))
import Language.Daicker.Span (Span (FixtureSpan), mkSpan, union)
import qualified Language.Daicker.Span as S
import System.Directory (getCurrentDirectory)
import System.Exit (ExitCode (ExitFailure, ExitSuccess), exitFailure)
import System.IO (hSetBuffering)
import qualified System.IO as IO
import System.Process

eval :: Bundle a -> Expr a -> ExceptT (RuntimeError a) IO (Expr a)
eval bundle@(Bundle p ms c ss as) v = case v of
  e@(s :< EError {}) -> pure e
  s :< EArray vs -> (:<) s . EArray <$> mapM (eval bundle) vs
  s :< EObject vs -> (:<) s . EObject <$> mapM (\(k, e) -> (,) k <$> eval bundle e) vs
  s :< EVar i -> case findExprWithBundle bundle i of
    Right (a, bundle) -> eval bundle a
    Left ((StaticError m s) : _) -> throwError $ RuntimeError m s (ExitFailure 1)
  s :< ECall f args -> do
    args <-
      mapM
        ( \case
            (s :< PositionedArgument e) -> (s :<) . PositionedArgument <$> eval bundle e
            (s :< KeywordArgument i e) -> (s :<) . KeywordArgument i <$> eval bundle e
        )
        args
    f' <- eval bundle f
    case f' of
      (s :< ELambda pms (s' :< e) t) -> do
        args <- zipArg pms args
        eval (Bundle p ms c ss (as <> args)) (s' :< e)
      (s :< EFixtureFun pms e ex) -> do
        args <- zipArg pms args
        liftIO $ e s args
      (s :< e) ->
        case args of
          [] -> pure $ s :< e
          _ -> throwError $ RuntimeError "Not a function" s (ExitFailure 1)
  s :< EAccessor e key -> do
    e <- eval bundle e
    case e of
      (_ :< EObject vs) -> case find (\(_, _) -> undefined) vs of
        Just (_, v) -> pure v
        Nothing -> pure $ s :< ENull
      (s :< _) -> throwError $ RuntimeError "Accessors can only be used on objects" s (ExitFailure 1)
  v -> pure v
  where
    expand (_ :< EArray es) = es

zipArg :: [Parameter ann] -> [Argument ann] -> ExceptT (RuntimeError ann) IO [(String, Expr ann)]
zipArg params args =
  (<>)
    <$> liftEither (zipPositionedArg positionedParams positionedArgs)
    <*> liftEither (zipKeywordArg keywordParams keywordArgs)
  where
    positionedParams = filter isPositionedParam params
    keywordParams = filter isKeywordParam params
    positionedArgs = filter isPositionedArg args
    keywordArgs = filter isKeywordArg args
    isPositionedParam :: Parameter ann -> Bool
    isPositionedParam (_ :< PositionedParameter {}) = True
    isPositionedParam _ = False
    isKeywordParam :: Parameter ann -> Bool
    isKeywordParam (_ :< KeywordParameter {}) = True
    isKeywordParam _ = False
    isPositionedArg :: Argument ann -> Bool
    isPositionedArg (_ :< PositionedArgument {}) = True
    isPositionedArg _ = False
    isKeywordArg :: Argument ann -> Bool
    isKeywordArg (_ :< KeywordArgument {}) = True
    isKeywordArg _ = False
    zipPositionedArg :: [Parameter ann] -> [Argument ann] -> Either (RuntimeError ann) [(String, Expr ann)]
    zipPositionedArg (p : ps) ((_ :< PositionedArgument a) : as) = ((paramName p, a) :) <$> zipPositionedArg ps as
    zipPositionedArg [] _ = pure []
    zipKeywordArg :: [Parameter ann] -> [Argument ann] -> Either (RuntimeError ann) [(String, Expr ann)]
    zipKeywordArg (p@(_ :< KeywordParameter (s :< Identifier name) _ _ _ defaultValue) : ps) as = do
      value <- case find (\(_ :< KeywordArgument (_ :< Identifier name') e) -> name == name') as of
        Just (_ :< KeywordArgument _ a) -> pure a
        Nothing -> case defaultValue of
          Just e -> pure e
          Nothing -> Left $ RuntimeError ("Missing keyword argument: " <> name) s (ExitFailure 1)
      ((paramName p, value) :) <$> zipKeywordArg ps as
    zipKeywordArg [] _ = pure []
    paramName :: Parameter ann -> String
    paramName (_ :< PositionedParameter (i :< Identifier name) _ _ _ _) = name
    paramName (_ :< KeywordParameter (i :< Identifier name) _ _ _ _) = name
