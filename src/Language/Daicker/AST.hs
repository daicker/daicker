{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}

module Language.Daicker.AST where

import Control.Comonad.Cofree
import Control.Monad (void)
import Data.Aeson (FromJSON (parseJSON), FromJSONKey (), ToJSON (toJSON), Value (Array, Bool, Null, Object, String), (.:))
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import Data.Aeson.Types (Parser, Value (Number))
import Data.Scientific (Scientific, toRealFloat)
import Data.Text (pack)
import qualified Data.Text as T
import qualified Data.Vector as V
import Language.Daicker.Span (Span, Spanned, mkSpan, span)

data Module ann = Module (Identifier ann) [Import ann] [Export ann] [Define ann]

data Import ann = Import (Identifier ann) ann deriving (Show, Eq)

data Export ann = Export (Identifier ann) ann deriving (Show, Eq)

data Define ann = Define (Identifier ann) (Expr ann) ann

type Expr ann = Cofree (Expr' ann) ann

data Expr' ann a
  = ENull
  | EBool Bool
  | ENumber Double
  | EString String
  | EArray [a]
  | EObject [(EKey ann, a)]
  | ERef (Identifier ann)
  | EApp (Maybe (EImage ann)) [a]
  | EFun [EArg ann] a
  deriving (Show, Eq)

type EKey = Identifier

type EArg = Identifier

type EImage = Identifier

data Identifier ann = Identifier String ann deriving (Show, Eq)

instance Spanned (Expr Span) where
  span :: Expr Span -> Span
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
