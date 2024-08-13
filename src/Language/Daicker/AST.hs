{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}

module Language.Daicker.AST where

import Control.Comonad.Cofree
import Control.Monad (join, void)
import Control.Monad.Except (ExceptT)
import Data.Aeson (FromJSON (parseJSON), FromJSONKey (), ToJSON (toJSON), Value (Array, Bool, Null, Object, String), (.:))
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import Data.Aeson.Types (Parser, Value (Number))
import Data.Data (typeOf)
import Data.Functor.Classes (Eq1 (liftEq), Show1 (liftShowsPrec), Show2 (liftShowsPrec2))
import Data.Scientific (Scientific, toRealFloat)
import Data.Text (pack)
import qualified Data.Text as T
import qualified Data.Vector as V
import Language.Daicker.Error (RuntimeError)
import Language.Daicker.Span (Span, Spanned, mkSpan, span)
import Language.LSP.Protocol.Types (Uri)
import System.Exit (ExitCode)

type Module ann = Cofree (Module' ann) ann

data Module' ann a = Module [Import ann] (Maybe (Export ann)) [NamedStatement ann] deriving (Show, Eq)

instance (Eq ann) => Eq1 (Module' ann) where
  liftEq _ (Module i1 e1 s1) (Module i2 e2 s2) =
    i1 == i2 && e1 == e2 && s1 == s2

instance (Show ann) => Show1 (Module' ann) where
  liftShowsPrec _ _ _ (Module i e s) = showString $ "Module " <> show i <> show e <> show s

type Import ann = Cofree (Import' ann) ann

data Import' ann a
  = PartialImport [Identifier ann] (URL ann)
  | WildImport (URL ann)
  deriving (Show, Eq)

instance (Eq ann) => Eq1 (Import' ann) where
  liftEq _ (PartialImport i1 url1) (PartialImport i2 url2) = i1 == i2 && url1 == url2
  liftEq _ (WildImport url1) (WildImport url2) = url1 == url2
  liftEq _ _ _ = False

instance (Show ann) => Show1 (Import' ann) where
  liftShowsPrec _ _ _ (PartialImport is url) = showString $ "PartialImport " <> show is <> show url
  liftShowsPrec _ _ _ (WildImport url) = showString $ "WildImport " <> show url

type URL ann = Cofree (URL' ann) ann

data URL' ann a
  = LocalFile String
  | RemoteFile String
  deriving (Show, Eq)

instance (Eq ann) => Eq1 (URL' ann) where
  liftEq _ (LocalFile url1) (LocalFile url2) = url1 == url2
  liftEq _ (RemoteFile url1) (RemoteFile url2) = url1 == url2
  liftEq _ _ _ = False

instance (Show ann) => Show1 (URL' ann) where
  liftShowsPrec _ _ _ (LocalFile url) = showString $ "LocalFile " <> show url
  liftShowsPrec _ _ _ (RemoteFile url) = showString $ "RemoteFile " <> show url

type Export ann = Cofree (Export' ann) ann

newtype Export' ann a = Export [Identifier ann] deriving (Show, Eq)

instance (Eq ann) => Eq1 (Export' ann) where
  liftEq _ (Export i1) (Export i2) = i1 == i2

instance (Show ann) => Show1 (Export' ann) where
  liftShowsPrec _ _ _ (Export i) = showString $ show "Export " <> show i

type NamedStatement ann = Cofree (NamedStatement' ann) ann

data NamedStatement' ann a = NamedStatement (Identifier ann) (Statement ann) deriving (Show, Eq)

instance (Eq ann) => Eq1 (NamedStatement' ann) where
  liftEq _ (NamedStatement i1 s1) (NamedStatement i2 s2) = i1 == i2 && s1 == s2

instance (Show ann) => Show1 (NamedStatement' ann) where
  liftShowsPrec _ _ _ (NamedStatement i s) = showString $ show "NamedStatement " <> show i <> show s

type Statement ann = Cofree (Statement' ann) ann

data Statement' ann a
  = SExpr (Expr ann)
  | SType (Type ann)
  deriving (Show, Eq)

instance (Eq ann) => Eq1 (Statement' ann) where
  liftEq _ (SExpr d1) (SExpr d2) = d1 == d2
  liftEq _ (SType d1) (SType d2) = d1 == d2

instance (Show ann) => Show1 (Statement' ann) where
  liftShowsPrec _ _ _ (SExpr d) = showString $ show "SExpr " <> show d
  liftShowsPrec _ _ _ (SType d) = showString $ show "SType " <> show d

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
  | EApp (Maybe (EImage ann)) a [(a, Expansion)]
  | EFun [PatternMatchAssign ann] a Expansion
  | ENamedExpr (Identifier ann) a
  | EError String ExitCode
  | EFixtureFun [PatternMatchAssign ann] (Maybe (EImage ann) -> [a] -> IO a) Expansion
  deriving (Show, Eq)

instance Show (Maybe (EImage ann) -> [a] -> IO a) where
  show :: (Maybe (EImage ann) -> [a] -> IO a) -> String
  show f = undefined

instance Eq (Maybe (EImage ann) -> [a] -> IO a) where
  (==) :: (Maybe (EImage ann) -> [a] -> IO a) -> (Maybe (EImage ann) -> [a] -> IO a) -> Bool
  _ == _ = undefined

type Expansion = Bool

instance (Eq ann) => Eq1 (Expr' ann) where
  liftEq _ ENull ENull = True
  liftEq _ (EBool a) (EBool b) = a == b
  liftEq _ (ENumber a) (ENumber b) = a == b
  liftEq _ (EString a) (EString b) = a == b
  liftEq f (EArray a) (EArray b) = length a == length b && all (uncurry f) (zip a b)
  liftEq f (EObject a) (EObject b) = length a == length b && all (\((ka, va), (kb, vb)) -> ka == kb && f va vb) (zip a b)
  liftEq _ (ERef a) (ERef b) = a == b
  liftEq f (EProperty a1 i1) (EProperty a2 i2) = f a1 a2 && i1 == i2
  liftEq f (EApp c1 f1 a1) (EApp c2 f2 a2) = c1 == c2 && f f1 f2 && length a1 == length a2 && all (\((a1', e1), (a2', e2)) -> f a1' a2' && e1 == e2) (zip a1 a2)
  liftEq f (EFun arg1 e1 ex1) (EFun arg2 e2 ex2) = arg1 == arg2 && f e1 e2 && ex1 == ex2
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
  liftShowsPrec f f' n (EApp img a as) = showString "EApp " <> showString (show img) <> f n a <> f' (map fst as)
  liftShowsPrec f _ n (EProperty a i) = showString "EProperty " <> f n a <> showString (show i)
  liftShowsPrec f _ n (EFun pma e ex) = showString "EFun " <> showString (show pma) <> f n e <> showString (show ex)

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

switchAnn :: (a -> b) -> Expr a -> Expr b
switchAnn f e = case e of
  (ann :< ENull) -> f ann :< ENull
  (ann :< EBool v) -> f ann :< EBool v
  (ann :< ENumber v) -> f ann :< ENumber v
  (ann :< EString v) -> f ann :< EString v
  (ann :< EArray es) -> f ann :< EArray (map (switchAnn f) es)
  (ann :< EObject es) -> f ann :< EObject (map (\(ann1 :< Identifier i, e) -> (f ann1 :< Identifier i, switchAnn f e)) es)
