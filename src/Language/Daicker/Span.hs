{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}

module Language.Daicker.Span where

import Language.LSP.Protocol.Types (Range, mkRange)
import Text.Megaparsec.Pos (SourcePos (SourcePos), mkPos, unPos)

data Position = Position
  { fileName :: FilePath,
    line :: Int,
    column :: Int
  }
  deriving (Show, Ord, Eq)

data Span
  = Span
      { startPos :: Position,
        endPos :: Position
      }
  | FixtureSpan String
  deriving (Show, Ord, Eq)

data WithSpan a = WithSpan
  { _value :: a,
    _span :: Span
  }
  deriving (Eq, Ord, Show)

class Spanned a where
  span :: a -> Span

instance Spanned (WithSpan a) where
  span :: WithSpan a -> Span
  span (WithSpan _ s) = s

union :: Span -> Span -> Span
union (Span p1 _) (Span _ p2) = Span p1 p2

instance Semigroup Span where
  (<>) = union

mkSpan :: FilePath -> Int -> Int -> Int -> Int -> Span
mkSpan file l1 c1 l2 c2 =
  Span
    (Position file l1 c1)
    (Position file l2 c2)

length :: Span -> Int
length (Span (Position _ _ c1) (Position _ _ c2)) = c2 - c1

toSourcePos :: Position -> SourcePos
toSourcePos (Position file l c) = SourcePos file (mkPos l) (mkPos c)

fromSourcePos :: SourcePos -> Position
fromSourcePos (SourcePos file l c) = Position file (unPos l) (unPos c)

toRange :: Span -> Range
toRange (Span p1@(Position _ l1 c1) p2@(Position _ l2 c2)) =
  if p1 == p2
    then
      mkRange
        (fromIntegral l1 - 1)
        (fromIntegral c1 - 1)
        (fromIntegral l2 - 1)
        (fromIntegral c2)
    else
      mkRange
        (fromIntegral l1 - 1)
        (fromIntegral c1 - 1)
        (fromIntegral l2 - 1)
        (fromIntegral c2 - 1)

initialPos :: FilePath -> Position
initialPos file = Position file 1 1

spanPretty :: Span -> String
spanPretty (Span p1@(Position file l1 c1) p2@(Position _ l2 c2)) =
  if p1 == p2
    then file <> ":" <> show l1 <> ":" <> show c1
    else file <> ":" <> show l1 <> ":" <> show c1 <> "-" <> show l2 <> ":" <> show c2
spanPretty (FixtureSpan s) = s
