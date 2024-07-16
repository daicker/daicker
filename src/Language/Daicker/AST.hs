{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}

module Language.Daicker.AST where

import Control.Comonad.Cofree
import Control.Monad (join, void)
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

type Module ann = Cofree (Module' ann) ann

data Module' ann a = Module (Identifier ann) [Import ann] [Export ann] [Define ann] deriving (Show, Eq)

instance (Eq ann) => Eq1 (Module' ann) where
  liftEq _ (Module i1 imports1 exports1 define1) (Module i2 imports2 exports2 define2) =
    i1 == i2 && imports1 == imports2 && exports1 == exports2 && define1 == define2

instance (Show ann) => Show1 (Module' ann) where
  liftShowsPrec _ _ _ (Module name i e d) = showString $ "Module " <> show name <> show i <> show e <> show d

type Import ann = Cofree (Import' ann) ann

newtype Import' ann a = Import (Identifier ann) deriving (Show, Eq)

instance (Eq ann) => Eq1 (Import' ann) where
  liftEq _ (Import i1) (Import i2) = i1 == i2

instance (Show ann) => Show1 (Import' ann) where
  liftShowsPrec _ _ _ (Import i) = showString $ "Import " <> show i

type Export ann = Cofree (Export' ann) ann

newtype Export' ann a = Export (Identifier ann) deriving (Show, Eq)

instance (Eq ann) => Eq1 (Export' ann) where
  liftEq _ (Export i1) (Export i2) = i1 == i2

instance (Show ann) => Show1 (Export' ann) where
  liftShowsPrec _ _ _ (Export i) = showString $ "Export " <> show i

type Define ann = Cofree (Define' ann) ann

data Define' ann a = Define (Identifier ann) (Expr ann) (Maybe (Type ann)) deriving (Show, Eq)

instance (Eq ann) => Eq1 (Define' ann) where
  liftEq _ (Define i1 e1 t1) (Define i2 e2 t2) = i1 == i2 && e1 == e2 && t1 == t2

instance (Show ann) => Show1 (Define' ann) where
  liftShowsPrec _ _ _ (Define i e t) = showString $ show i <> show e <> show t

type TypeDefine ann = Cofree (TypeDefine' ann) ann

data TypeDefine' ann a = TypeDefine (Identifier ann) (Type ann) deriving (Show, Eq)

instance (Eq ann) => Eq1 (TypeDefine' ann) where
  liftEq _ (TypeDefine i1 e1) (TypeDefine i2 e2) = i1 == i2 && e1 == e2

instance (Show ann) => Show1 (TypeDefine' ann) where
  liftShowsPrec _ _ _ (TypeDefine i e) = showString $ show i <> show e

type Type ann = Cofree (Type' ann) ann

data Type' ann a
  = TVoid
  | TNull
  | TBool
  | TNumber
  | TString
  | TTuple [a]
  | TArray a
  | TObject [(EKey ann, a)]
  | TMap a
  | TFun a a
  | TRef (Identifier ann)
  deriving (Show, Eq)

instance (Eq ann) => Eq1 (Type' ann) where
  liftEq _ TVoid TVoid = True
  liftEq _ TNull TNull = True
  liftEq _ TBool TBool = True
  liftEq _ TNumber TNumber = True
  liftEq _ TString TString = True
  liftEq f (TTuple as) (TTuple bs) = length as == length bs && all (uncurry f) (zip as bs)
  liftEq f (TArray a) (TArray b) = f a b
  liftEq f (TObject as) (TObject bs) = length as == length bs && all (\((ka, va), (kb, vb)) -> ka == kb && f va vb) (zip as bs)
  liftEq f (TMap a) (TMap b) = f a b
  liftEq f (TFun f1 a1) (TFun f2 a2) = f f1 f2 && f a1 a2
  liftEq _ (TRef n1) (TRef n2) = n1 == n2

instance (Show ann) => Show1 (Type' ann) where
  liftShowsPrec _ _ _ TVoid = showString "TVoid"
  liftShowsPrec _ _ _ TNull = showString "TNull"
  liftShowsPrec _ _ _ TBool = showString "TBool"
  liftShowsPrec _ _ _ TNumber = showString "TNumber"
  liftShowsPrec _ _ _ TString = showString "TString"
  liftShowsPrec _ f _ (TTuple ts) = showString "TTuple " <> f ts
  liftShowsPrec f _ n (TArray a) = showString "TArray " <> f n a
  liftShowsPrec f _ n (TObject as) =
    showString "TObject ["
      <> foldl1
        (\a b -> a <> showString ", " <> b)
        (map (\(i, a) -> showString "(" <> showString (show i) <> showString ", " <> f n a <> showString ")") as)
      <> showString "]"
  liftShowsPrec f _ n (TMap a) = showString "TMap " <> f n a
  liftShowsPrec f _ n (TFun a b) = showString "TFun " <> f n a <> f n b
  liftShowsPrec f _ n (TRef name) = showString $ "TRef " <> show name

type Expr ann = Cofree (Expr' ann) ann

data Expr' ann a
  = ENull
  | EBool Bool
  | ENumber Scientific
  | EString String
  | EArray [a]
  | EObject [(EKey ann, a)]
  | ERef (Identifier ann)
  | EProperty a (Identifier ann)
  | EElement a (Index ann)
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
  liftEq f (EProperty a1 i1) (EProperty a2 i2) = f a1 a2 && i1 == i2
  liftEq f (EApp c1 f1 a1) (EApp c2 f2 a2) = c1 == c2 && f f1 f2 && f a1 a2
  liftEq f (EFun arg1 e1) (EFun arg2 e2) = arg1 == arg2 && f e1 e2
  liftEq _ _ _ = False

instance (Show ann) => Show1 (Expr' ann) where
  liftShowsPrec _ _ _ ENull = showString "ENull"
  liftShowsPrec _ _ _ (EBool v) = showString $ "EBool " <> show v
  liftShowsPrec _ _ _ (ENumber v) = showString $ "ENumber " <> show v
  liftShowsPrec _ _ _ (EString v) = showString $ "EString " <> show v
  liftShowsPrec _ f _ (EArray vs) = showString "EArray " <> f vs
  liftShowsPrec f _ n (EObject vs) =
    showString "EObject ["
      <> foldl1
        (\a b -> a <> showString ", " <> b)
        (map (\(i, a) -> showString "(" <> showString (show i) <> showString ", " <> f n a <> showString ")") vs)
      <> showString "]"
  liftShowsPrec _ _ _ (ERef i) = showString "ERef " <> showString (show i)
  liftShowsPrec f _ n (EApp img a b) = showString "EApp " <> showString (show img) <> f n a <> f n b
  liftShowsPrec f _ n (EProperty a i) = showString "EProperty " <> f n a <> showString (show i)
  liftShowsPrec f _ n (EFun pma e) = showString "EFun " <> showString (show pma) <> f n e

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
  liftShowsPrec _ _ _ (PMAAnyValue i) = showString $ "PMAAnyValue " <> show i
  liftShowsPrec _ f _ (PMAArray vs) = showString "PMAArray " <> f vs
  liftShowsPrec f _ n (PMAObject vs) =
    showString "PMAObject ["
      <> foldl1
        (\a b -> a <> showString ", " <> b)
        (map (\(i, a) -> showString "(" <> showString (show i) <> showString ", " <> f n a <> showString ")") vs)
      <> showString "]"

type EKey ann = Identifier ann

type EArg ann = Identifier ann

type EImage ann = Identifier ann

type Identifier ann = Cofree (Identifier' ann) ann

newtype Identifier' ann a = Identifier String deriving (Show, Eq)

instance (Eq ann) => Eq1 (Identifier' ann) where
  liftEq _ (Identifier i1) (Identifier i2) = i1 == i2

instance (Show ann) => Show1 (Identifier' ann) where
  liftShowsPrec _ _ _ (Identifier i) = showString $ "Identifier " <> i

type Index ann = Cofree (Index' ann) ann

newtype Index' ann a = Index Int deriving (Show, Eq)

instance (Eq ann) => Eq1 (Index' ann) where
  liftEq _ (Index i1) (Index i2) = i1 == i2

instance (Show ann) => Show1 (Index' ann) where
  liftShowsPrec _ _ _ (Index i) = showString $ "Index " <> show i

instance Spanned (Type Span) where
  span :: Type Span -> Span
  span (s :< _) = s

instance Spanned (Expr Span) where
  span :: Expr Span -> Span
  span (s :< _) = s

instance Spanned (PatternMatchAssign Span) where
  span :: PatternMatchAssign Span -> Span
  span (s :< _) = s

instance Spanned (Identifier Span) where
  span :: Identifier Span -> Span
  span (s :< Identifier _) = s

instance ToJSON (Expr a) where
  toJSON :: Expr a -> Value
  toJSON expr = case expr of
    (_ :< ENull) -> Null
    (_ :< EBool v) -> Bool v
    (_ :< ENumber v) -> Number v
    (_ :< EString v) -> String (pack v)
    (_ :< EArray vs) -> Array $ V.fromList $ map toJSON vs
    (_ :< EObject vs) -> Object $ KM.fromList $ map (\(s :< Identifier i, v) -> (K.fromText (pack i), toJSON v)) vs
    (_ :< v) -> String (pack "not value")

instance FromJSON (Expr ()) where
  parseJSON :: Value -> Parser (Expr ())
  parseJSON v = case v of
    Null -> pure $ () :< ENull
    Bool v -> pure $ () :< EBool v
    Number v -> pure $ () :< ENumber v
    String v -> pure $ () :< EString (T.unpack v)
    Array vs -> (() :<) . EArray <$> mapM parseJSON (V.toList vs)
    Object vs ->
      (() :<) . EObject
        <$> mapM
          (\(k, v) -> (,) (() :< Identifier (K.toString k)) <$> parseJSON v)
          (KM.toList vs)
