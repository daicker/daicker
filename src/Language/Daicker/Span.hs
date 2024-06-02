module Language.Daicker.Span where
import Text.Megaparsec.Pos ( SourcePos(SourcePos), mkPos )

data Span = Span SourcePos SourcePos deriving (Show, Eq)

mkSpan :: FilePath -> Int -> Int -> Int -> Int -> Span
mkSpan file l1 c1 l2 c2 = Span
  (SourcePos file (mkPos l1) (mkPos c1))
  (SourcePos file (mkPos l2) (mkPos c2))
