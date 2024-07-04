{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}

module Language.Daicker.AST where

import Control.Comonad.Cofree
import Control.Monad (void)
import Data.Aeson (FromJSON (parseJSON), FromJSONKey (), ToJSON (toJSON), Value (Array, Bool, Null, Object, String), (.:))
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import Data.Aeson.Types (Parser, Value (Number))
import Data.Functor.Classes (Eq1 (liftEq), Show1 (liftShowsPrec), Show2 (liftShowsPrec2))
import Data.Scientific (Scientific, toRealFloat)
import Data.Text (pack)
import qualified Data.Text as T
import qualified Data.Vector as V
import Language.Daicker.Span (Span, Spanned, mkSpan, span)

data Module ann = Module (Identifier ann) [Import ann] [Export ann] [Define ann] deriving (Show, Eq)

data Import ann = Import (Identifier ann) ann deriving (Show, Eq)

data Export ann = Export (Identifier ann) ann deriving (Show, Eq)

data Define ann = Define (Identifier ann) (Expr ann) ann deriving (Show, Eq)

type Expr ann = Cofree (Expr' ann) ann

data Expr' ann a
  = ENull
  | EBool Bool
  | ENumber Double
  | EString String
  | EArray [a]
  | EObject [(EKey ann, a)]
  | ERef (Identifier ann)
  | EApp (Maybe (EImage ann)) a a
  | EFun (Maybe (PatternMatchAssign ann)) a
  deriving (Show, Eq)

instance (Eq ann) => Eq1 (Expr' ann) where
  liftEq _ ENull ENull = True
  liftEq _ (EBool a) (EBool b) = a == b
  liftEq _ (ENumber a) (ENumber b) = a == b
  liftEq _ (EString a) (EString b) = a == b
  liftEq f (EArray a) (EArray b) = length a == length b && all (uncurry f) (zip a b)
  liftEq f (EObject a) (EObject b) = length a == length b && all (\((ka, va), (kb, vb)) -> ka == kb && f va vb) (zip a b)
  liftEq _ (ERef a) (ERef b) = a == b
  liftEq f (EApp c1 f1 a1) (EApp c2 f2 a2) = c1 == c2 && f f1 f2 && f a1 a2
  liftEq f (EFun arg1 e1) (EFun arg2 e2) = arg1 == arg2 && f e1 e2
  liftEq _ _ _ = False

instance (Show ann) => Show1 (Expr' ann) where
  liftShowsPrec _ _ _ ENull = showString "ENull"
  liftShowsPrec _ _ _ (EBool v) = showString $ "EBool " <> show v
  liftShowsPrec _ _ _ (ENumber v) = showString $ "ENumber " <> show v
  liftShowsPrec _ _ _ (EString v) = showString $ "EString " <> show v
  liftShowsPrec _ f _ (EArray vs) = showString "EArray " <> f vs
  liftShowsPrec _ _ _ (EObject vs) = showString "EObject..."
  liftShowsPrec _ _ _ (EApp {}) = showString "EApp..."
  liftShowsPrec _ _ _ (EFun {}) = showString "EFun..."

type PatternMatchAssign ann = Cofree (PatternMatchAssign' ann) ann

data PatternMatchAssign' ann a
  = PMAAnyValue (Identifier ann)
  | PMAArray [a]
  | PMAObject [(EKey ann, a)]
  deriving (Show, Eq)

instance (Eq ann) => Eq1 (PatternMatchAssign' ann) where
  liftEq _ (PMAAnyValue a) (PMAAnyValue b) = a == b
  liftEq f (PMAArray as) (PMAArray bs) = length as == length bs && all (uncurry f) (zip as bs)
  liftEq f (PMAObject as) (PMAObject bs) = length as == length bs && all (\((k1, v1), (k2, v2)) -> k1 == k2 && f v1 v2) (zip as bs)
  liftEq _ _ _ = False

instance (Show ann) => Show1 (PatternMatchAssign' ann) where
  liftShowsPrec _ _ _ _ = showString "PatternMatchAssign"

type EKey = Identifier

type EArg = Identifier

type EImage = Identifier

data Identifier ann = Identifier String ann deriving (Show, Eq)

instance Spanned (Expr Span) where
  span :: Expr Span -> Span
  span (s :< _) = s

instance Spanned (PatternMatchAssign Span) where
  span :: PatternMatchAssign Span -> Span
  span (s :< _) = s

instance Spanned (Identifier Span) where
  span :: Identifier Span -> Span
  span (Identifier _ s) = s

instance ToJSON (Expr a) where
  toJSON :: Expr a -> Value
  toJSON expr = case expr of
    (_ :< ENull) -> Null
    (_ :< EBool v) -> Bool v
    (_ :< ENumber v) -> Number (read (show v) :: Scientific)
    (_ :< EString v) -> String (pack v)
    (_ :< EArray vs) -> Array $ V.fromList $ map toJSON vs
    (_ :< EObject vs) -> Object $ KM.fromList $ map (\(Identifier i _, v) -> (K.fromText (pack i), toJSON v)) vs
    (_ :< v) -> String (pack "not value")

instance FromJSON (Expr ()) where
  parseJSON :: Value -> Parser (Expr ())
  parseJSON v = case v of
    Null -> pure $ () :< ENull
    Bool v -> pure $ () :< EBool v
    Number v -> pure $ () :< ENumber (toRealFloat v)
    String v -> pure $ () :< EString (T.unpack v)
    Array vs -> (() :<) . EArray <$> mapM parseJSON (V.toList vs)
    Object vs ->
      (() :<) . EObject
        <$> mapM
          (\(k, v) -> (,) (Identifier (K.toString k) ()) <$> parseJSON v)
          (KM.toList vs)
