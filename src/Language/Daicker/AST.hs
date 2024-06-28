{-# LANGUAGE InstanceSigs #-}

module Language.Daicker.AST where

import Data.Aeson (FromJSON (parseJSON), FromJSONKey (), ToJSON (toJSON), Value (Array, Bool, Null, Object, String), (.:))
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import Data.Aeson.Types (Parser, Value (Number))
import Data.Scientific (Scientific, toRealFloat)
import Data.Text (pack)
import qualified Data.Text as T
import qualified Data.Vector as V
import Language.Daicker.Span (Span, Spanned, mkSpan, span)

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
    EObject vs _ -> Object $ KM.fromList $ map (\(Identifier i _, v) -> (K.fromText (pack i), toJSON v)) vs
    v -> String (pack $ show v)

instance FromJSON Expr where
  parseJSON :: Value -> Parser Expr
  parseJSON v = case v of
    Null -> pure $ ENull $ mkSpan "stdin" 1 1 1 2
    Bool v -> pure $ EBool v $ mkSpan "stdin" 1 1 1 2
    Number v -> pure $ ENumber (toRealFloat v) $ mkSpan "stdin" 1 1 1 2
    String v -> pure $ EString (T.unpack v) $ mkSpan "stdin" 1 1 1 2
    Array vs -> EArray <$> mapM parseJSON (V.toList vs) <*> pure (mkSpan "stdin" 1 1 1 2)
    Object vs ->
      EObject
        <$> mapM
          (\(k, v) -> (,) (Identifier (K.toString k) (mkSpan "stdin" 1 1 1 2)) <$> parseJSON v)
          (KM.toList vs)
        <*> pure (mkSpan "stdin" 1 1 1 2)
