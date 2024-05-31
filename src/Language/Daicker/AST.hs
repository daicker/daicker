module Language.Daicker.AST where
import Language.Daicker.Span (Span)

data Module = Module Identifier [Import] [Export] [Define]

data Import = Import Identifier Span deriving (Show)
data Export = Export Identifier Span deriving (Show)
data Define = Define Identifier Value Span deriving (Show)

data Value
  = VNull Span
  | VBool Bool Span
  | VNumber Double Span
  | VString String Span
  | VArray [Value] Span
  | VObject [(String, Value)] Span
  | VRef Identifier Span
  | VApp Value [Value] Span
  | VFun [VArg] Value Span
  deriving (Show)

type VArg = Identifier
data Identifier = Identifier String Span deriving (Show)
