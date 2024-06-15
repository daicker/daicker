{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}

module Language.Daicker.Span where

import Text.Megaparsec.Pos (SourcePos (SourcePos), mkPos, unPos)

data Span = Span SourcePos SourcePos deriving (Show, Eq)

class Spanned a where
  span :: a -> Span

instance Spanned (a, Span) where
  span :: (a, Span) -> Span
  span (_, s) = s

(<>) :: Span -> Span -> Span
(Span p1 _) <> (Span _ p2) = Span p1 p2

mkSpan :: FilePath -> Int -> Int -> Int -> Int -> Span
mkSpan file l1 c1 l2 c2 =
  Span
    (SourcePos file (mkPos l1) (mkPos c1))
    (SourcePos file (mkPos l2) (mkPos c2))

length :: Span -> Int
length (Span (SourcePos _ _ c1) (SourcePos _ _ c2)) = unPos c2 - unPos c1

diff :: Span -> (Int, Int)
diff (Span (SourcePos _ l1 c1) (SourcePos _ l2 c2)) = (unPos l2 - unPos l1, unPos c2 - unPos c1)
