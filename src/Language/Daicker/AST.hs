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

data Module' ann a = Module [Import ann] (Maybe (Export ann)) [Statement ann] deriving (Show, Eq)

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

type Statement ann = Cofree (Statement' ann) ann

data Statement' ann a
  = SExpr (Identifier ann) (Expr ann)
  | SType (Identifier ann) [Identifier ann] (Type ann)
  deriving (Show, Eq)

instance (Eq ann) => Eq1 (Statement' ann) where
  liftEq _ (SExpr i1 d1) (SExpr i2 d2) = i1 == i2 && d1 == d2
  liftEq _ (SType i1 a1 d1) (SType i2 a2 d2) = i1 == i2 && a1 == a2 && d1 == d2
  liftEq _ _ _ = False

instance (Show ann) => Show1 (Statement' ann) where
  liftShowsPrec _ _ _ (SExpr i d) = showString $ show "SExpr " <> show i <> show d
  liftShowsPrec _ _ _ (SType i a d) = showString $ show "SType " <> show i <> show a <> show d

(~=) :: Statement a -> Statement a -> Bool
(_ :< SExpr {}) ~= (_ :< SExpr {}) = True
(_ :< SType {}) ~= (_ :< SType {}) = True
_ ~= _ = False

type Type ann = Cofree (Type' ann) ann

data Type' ann a
  = TVar (Identifier ann)
  | TObject [(a, a)]
  | TParameterized a [a]
  | TStringLiteral String
  | TNumberLiteral Scientific
  | TBoolLiteral Bool
  | TNullLiteral
  deriving (Show, Eq)

instance (Eq ann) => Eq1 (Type' ann) where
  liftEq f (TObject as) (TObject bs) = length as == length bs && all (\((ka, va), (kb, vb)) -> f ka kb && f va vb) (zip as bs)
  liftEq _ (TVar n1) (TVar n2) = n1 == n2
  liftEq f (TParameterized f1 a1) (TParameterized f2 a2) = f f1 f2 && length a1 == length a2 && all (uncurry f) (zip a1 a2)
  liftEq _ (TStringLiteral s1) (TStringLiteral s2) = s1 == s2
  liftEq _ (TNumberLiteral n1) (TNumberLiteral n2) = n1 == n2
  liftEq _ (TBoolLiteral b1) (TBoolLiteral b2) = b1 == b2
  liftEq _ TNullLiteral TNullLiteral = True
  liftEq _ _ _ = False

instance (Show ann) => Show1 (Type' ann) where
  liftShowsPrec f _ n (TObject as) =
    showString "TObject ["
      <> foldl1
        (\a b -> a <> showString ", " <> b)
        (map (\(i, a) -> showString "(" <> f n i <> showString ", " <> f n a <> showString ")") as)
      <> showString "]"
  liftShowsPrec f _ n (TVar name) = showString $ "TVar " <> show name
  liftShowsPrec f f' n (TParameterized tf args) = showString "TCall " <> f n tf <> f' args
  liftShowsPrec _ _ _ (TStringLiteral s) = showString $ "TStringLiteral " <> s
  liftShowsPrec _ _ _ (TNumberLiteral n) = showString $ "TNumberLiteral " <> show n
  liftShowsPrec _ _ _ (TBoolLiteral b) = showString $ "TBoolLiteral " <> show b
  liftShowsPrec _ _ _ TNullLiteral = showString "TNullLiteral"

type TypeParameter ann = Identifier ann

type Expr ann = Cofree (Expr' ann) ann

data Expr' ann a
  = ENull
  | EBool Bool
  | ENumber Scientific
  | EString String
  | EArray [a]
  | EObject [(a, a)]
  | EImage (Identifier ann)
  | EVar (Identifier ann)
  | EAccessor a a
  | ECall a [Argument ann]
  | ELambda [Parameter ann] a (Maybe (Type ann))
  | EError String ExitCode
  | EFixtureFun [Parameter ann] ([Argument ann] -> IO a)
  deriving (Show, Eq)

instance Show ([Argument ann] -> IO a) where
  show :: ([Argument ann] -> IO a) -> String
  show f = undefined

instance Eq ([Argument ann] -> IO a) where
  (==) :: ([Argument ann] -> IO a) -> ([Argument ann] -> IO a) -> Bool
  _ == _ = undefined

instance (Eq ann) => Eq1 (Expr' ann) where
  liftEq _ ENull ENull = True
  liftEq _ (EBool a) (EBool b) = a == b
  liftEq _ (ENumber a) (ENumber b) = a == b
  liftEq _ (EString a) (EString b) = a == b
  liftEq f (EArray a) (EArray b) = length a == length b && all (uncurry f) (zip a b)
  liftEq f (EObject a) (EObject b) = length a == length b && all (\((ka, va), (kb, vb)) -> f ka kb && f va vb) (zip a b)
  liftEq _ (EImage a) (EImage b) = a == b
  liftEq _ (EVar a) (EVar b) = a == b
  liftEq f (EAccessor a1 i1) (EAccessor a2 i2) = f a1 a2 && f i1 i2
  liftEq f (ECall f1 a1) (ECall f2 a2) = f f1 f2 && length a1 == length a2 && all (uncurry (==)) (zip a1 a2)
  liftEq f (ELambda p1 e1 t1) (ELambda p2 e2 t2) = p1 == p2 && f e1 e2 && t1 == t2
  liftEq f (EError s1 c1) (EError s2 c2) = s1 == s2 && c1 == c2
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
        (map (\(i, a) -> showString "(" <> f n i <> showString ", " <> f n a <> showString ")") vs)
      <> showString "]"
  liftShowsPrec _ _ _ (EImage i) = showString "EImage " <> showString (show i)
  liftShowsPrec _ _ _ (EVar i) = showString "EVar " <> showString (show i)
  liftShowsPrec f f' n (ECall a as) = showString "ECall " <> f n a <> showString (show as)
  liftShowsPrec f _ n (EAccessor a i) = showString "EAccessor " <> f n a <> f n i
  liftShowsPrec f _ n (ELambda pma e t) = showString "ELambda " <> showString (show pma) <> f n e <> showString (show t)
  liftShowsPrec _ _ _ (EError s c) = showString $ "EError " <> s <> show c

type Parameter ann = Cofree (Parameter' ann) ann

data Parameter' ann a
  = PositionedParameter (Identifier ann) IsRest IsOptional (Maybe (Type ann)) (Maybe (Expr ann))
  | KeywordParameter (Identifier ann) IsRest IsOptional (Maybe (Type ann)) (Maybe (Expr ann))
  deriving (Show, Eq)

type IsRest = Bool -- Rest Parameter

type IsOptional = Bool -- Optional Parameter

instance (Eq ann) => Eq1 (Parameter' ann) where
  liftEq _ (PositionedParameter p1 r1 o1 t1 e1) (PositionedParameter p2 r2 o2 t2 e2) = p1 == p2 && r1 == r2 && o1 == o2 && t1 == t2 && e1 == e2
  liftEq _ (KeywordParameter i1 r1 o1 t1 e1) (KeywordParameter i2 r2 o2 t2 e2) = i1 == i2 && r1 == r2 && o1 == o2 && t1 == t2 && e1 == e2
  liftEq _ _ _ = False

instance (Show ann) => Show1 (Parameter' ann) where
  liftShowsPrec f _ n (PositionedParameter p r o t e) = showString "PositionedParameter " <> showString (show p) <> showString (show o) <> showString (show t) <> showString (show e)
  liftShowsPrec f _ n (KeywordParameter i r o t e) = showString "KeywordParameter " <> showString (show i) <> showString (show o) <> showString (show t) <> showString (show e)

type Argument ann = Cofree (Argument' ann) ann

data Argument' ann a
  = PositionedArgument (Expr ann)
  | KeywordArgument (Identifier ann) (Expr ann)
  deriving (Show, Eq)

instance (Eq ann) => Eq1 (Argument' ann) where
  liftEq f (PositionedArgument a1) (PositionedArgument a2) = a1 == a2
  liftEq f (KeywordArgument i1 a1) (KeywordArgument i2 a2) = i1 == i2 && a1 == a2
  liftEq _ _ _ = False

instance (Show ann) => Show1 (Argument' ann) where
  liftShowsPrec f _ n (PositionedArgument a) = showString "PositionedArgument " <> showString (show a)
  liftShowsPrec f _ n (KeywordArgument i a) = showString "KeywordArgument " <> showString (show i) <> showString (show a)

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
    (_ :< EObject vs) -> Object $ KM.fromList $ map (\(s :< EString i, v) -> (K.fromText (pack i), toJSON v)) vs
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
          (\(k, v) -> (,) (() :< EString (K.toString k)) <$> parseJSON v)
          (KM.toList vs)

switchAnn :: (a -> b) -> Expr a -> Expr b
switchAnn f e = case e of
  (ann :< ENull) -> f ann :< ENull
  (ann :< EBool v) -> f ann :< EBool v
  (ann :< ENumber v) -> f ann :< ENumber v
  (ann :< EString v) -> f ann :< EString v
  (ann :< EArray es) -> f ann :< EArray (map (switchAnn f) es)
  (ann :< EObject es) -> f ann :< EObject (map (\(ann1 :< EString i, e) -> (f ann1 :< EString i, switchAnn f e)) es)
