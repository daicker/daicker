module Language.Daicker.AST where
import Language.Daicker.Span (Span)

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
  | VObject [(String, Value)] Span
  | VRef Identifier Span
  | VApp Value [Value] Span
  | VFun [VArg] Value Span
  deriving (Show, Eq)

type VArg = Identifier
data Identifier = Identifier String Span deriving (Show, Eq)
