module Language.Daicker.Span where
import Text.Megaparsec.Pos (SourcePos)

data Span = Span SourcePos SourcePos deriving (Show)
