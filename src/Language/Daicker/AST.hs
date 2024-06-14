{-# LANGUAGE InstanceSigs #-}

module Language.Daicker.AST where

import Language.Daicker.Span (Span, Spanned, span)

data Module = Module Identifier [Import] [Export] [Define]

data Import = Import Identifier Span deriving (Show, Eq)

data Export = Export Identifier Span deriving (Show, Eq)

data Define = Define Identifier Value Span deriving (Show, Eq)

data Value
  = VNull Span
  | VBool Bool Span
  | VNumber Double Span
  | VString String Span
  | VArray [Value] Span
  | VObject [(VKey, Value)] Span
  | VRef Identifier Span
  | VApp (Maybe VImage) [Value] Span
  | VFun [VArg] Value Span
  deriving (Show, Eq)

type VKey = Identifier

type VArg = Identifier

type VImage = Identifier

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

instance Spanned Value where
  span :: Value -> Span
  span (VNull s) = s
  span (VBool _ s) = s
  span (VNumber _ s) = s
  span (VString _ s) = s
  span (VArray _ s) = s
  span (VObject _ s) = s
  span (VRef _ s) = s
  span (VApp _ _ s) = s
  span (VFun _ _ s) = s

instance Spanned Identifier where
  span :: Identifier -> Span
  span (Identifier _ s) = s
