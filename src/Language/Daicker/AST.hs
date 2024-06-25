{-# LANGUAGE InstanceSigs #-}

module Language.Daicker.AST where

import Data.Aeson (FromJSONKey (), ToJSON (toJSON), Value (Array, Bool, Null, Object, String))
import Data.Aeson.Key (fromText)
import qualified Data.Aeson.KeyMap as KM
import Data.Aeson.Types (Value (Number))
import Data.Scientific (Scientific)
import Data.Text (pack)
import qualified Data.Vector as V
import Language.Daicker.Span (Span, Spanned, span)

data Module = Module Identifier [Import] [Export] [Define]

data Import = Import Identifier Span deriving (Show, Eq)

data Export = Export Identifier Span deriving (Show, Eq)

data Define = Define Identifier Expr Span deriving (Show, Eq)

data Expr
  = ENull Span
  | EBool Bool Span
  | ENumber Double Span
  | EString String Span
  | EArray [Expr] Span
  | EObject [(EKey, Expr)] Span
  | ERef Identifier Span
  | EApp (Maybe EImage) [Expr] Span
  | EFun [EArg] Expr Span
  deriving (Show, Eq)

type EKey = Identifier

type EArg = Identifier

type EImage = Identifier

data Identifier = Identifier String Span deriving (Show, Eq)

instance Spanned Import where
  span :: Import -> Span
  span (Import _ s) = s

instance Spanned Export where
  span :: Export -> Span
  span (Export _ s) = s

instance Spanned Define where
  span :: Define -> Span
  span (Define _ _ s) = s

instance Spanned Expr where
  span :: Expr -> Span
  span (ENull s) = s
  span (EBool _ s) = s
  span (ENumber _ s) = s
  span (EString _ s) = s
  span (EArray _ s) = s
  span (EObject _ s) = s
  span (ERef _ s) = s
  span (EApp _ _ s) = s
  span (EFun _ _ s) = s

instance Spanned Identifier where
  span :: Identifier -> Span
  span (Identifier _ s) = s

instance ToJSON Expr where
  toJSON :: Expr -> Value
  toJSON expr = case expr of
    ENull _ -> Null
    EBool v _ -> Bool v
    ENumber v _ -> Number (read (show v) :: Scientific)
    EString v _ -> String (pack v)
    EArray vs _ -> Array $ V.fromList $ map toJSON vs
    EObject vs _ -> Object $ KM.fromList $ map (\(Identifier i _, v) -> (fromText (pack i), toJSON v)) vs
