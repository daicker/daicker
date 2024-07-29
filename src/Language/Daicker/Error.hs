{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}

module Language.Daicker.Error where

import Data.List (intercalate)
import qualified Data.List.NonEmpty as NEL
import Data.Void (Void)
import Language.Daicker.Span (Position (Position), Span (Span), spanPretty)
import System.Exit (ExitCode (ExitFailure), exitWith)
import System.IO (Handle, hPutStrLn)
import Text.Megaparsec (ParseErrorBundle, SourcePos (SourcePos), TraversableStream, VisualStream, errorBundlePretty, errorOffset, parseErrorPretty, parseErrorTextPretty)
import Text.Megaparsec.Error (ParseErrorBundle (..))
import Text.Megaparsec.Pos (unPos)
import Text.Megaparsec.State (PosState (..))
import Text.Megaparsec.Stream (TraversableStream (..))

-- StaticError is a lexical, syntax, type or semantic error.
data StaticError = StaticError String Span deriving (Show, Eq)

data RuntimeError = RuntimeError String Span ExitCode deriving (Show, Eq)

data CodeError
  = StaticE [StaticError]
  | RuntimeE RuntimeError
  deriving (Show, Eq)

fromParseErrorBundle :: (TraversableStream a, VisualStream a) => ParseErrorBundle a Void -> StaticError
fromParseErrorBundle e =
  StaticError
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

staticErrorPretty :: StaticError -> String
staticErrorPretty (StaticError m s) = spanPretty s <> ": " <> m

staticErrorListPretty :: [StaticError] -> String
staticErrorListPretty es = intercalate "\n" $ map staticErrorPretty es

runtimeErrorPretty :: RuntimeError -> String
runtimeErrorPretty (RuntimeError m s _) = spanPretty s <> ": " <> m

codeErrorPretty :: CodeError -> String
codeErrorPretty (StaticE es) = staticErrorListPretty es
codeErrorPretty (RuntimeE e) = runtimeErrorPretty e
