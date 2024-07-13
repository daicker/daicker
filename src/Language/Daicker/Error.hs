module Language.Daicker.Error where

import Data.List (intercalate)
import qualified Data.List.NonEmpty as NEL
import Data.Void (Void)
import Language.Daicker.Span (Position (Position), Span (Span), spanPretty)
import Text.Megaparsec (ParseErrorBundle, SourcePos (SourcePos), TraversableStream, VisualStream, errorBundlePretty, errorOffset, parseErrorPretty, parseErrorTextPretty)
import Text.Megaparsec.Error (ParseErrorBundle (..))
import Text.Megaparsec.Pos (unPos)
import Text.Megaparsec.State (PosState (..))
import Text.Megaparsec.Stream (TraversableStream (..))

data CodeError = CodeError String Span deriving (Show, Eq)

fromParseErrorBundle :: (TraversableStream a, VisualStream a) => ParseErrorBundle a Void -> CodeError
fromParseErrorBundle e =
  CodeError
    (parseErrorTextPretty $ NEL.head $ bundleErrors e)
    (mkSpan' $ errorBundleSourcePos e)
  where
    mkSpan' (SourcePos file l c) = Span (Position file (unPos l) (unPos c)) (Position file (unPos l) (unPos c))

errorBundleSourcePos :: (TraversableStream a) => ParseErrorBundle a Void -> SourcePos
errorBundleSourcePos peb = do
  let pst = bundlePosState peb
  let e = NEL.head $ bundleErrors peb
  let (_, pst') = reachOffset (errorOffset e) pst
  pstateSourcePos pst'

codeErrorPretty :: CodeError -> String
codeErrorPretty (CodeError m s) = spanPretty s <> ": " <> m

codeErrorListPretty :: [CodeError] -> String
codeErrorListPretty es = intercalate "\n" $ map codeErrorPretty es
