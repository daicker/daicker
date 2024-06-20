-- cf. https://markkarpov.com/tutorial/megaparsec.html#working-with-custom-input-streams
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}

module Language.Daicker.Lexer where

import Control.Monad (void)
import Data.Aeson (Value (Bool))
import qualified Data.List as DL
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import Data.Proxy
import Data.Scientific (toRealFloat)
import qualified Data.Set as Set
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char (alphaNumChar, char, lowerChar, newline, space1, string, upperChar)
import qualified Text.Megaparsec.Char.Lexer as L

data TToken
  = TNull
  | TBool Bool
  | TNumber Double
  | TString String
  | TIdentifier String
  | TEqual
  | TModule
  | TImport
  | TExport
  | TDefine
  | TLParenthesis
  | TRParenthesis
  | TLBracket
  | TRBracket
  | TLBrace
  | TRBrace
  | TArrow
  | TComma
  | TColon
  | TBackslash
  | TComment
  deriving (Eq, Ord, Show)

data WithPos a = WithPos
  { startPos :: SourcePos,
    endPos :: SourcePos,
    tokenLength :: Int,
    tokenVal :: a
  }
  deriving (Eq, Ord, Show)

data TStream = TStream
  { tStreamInput :: String, -- for showing offending lines
    unTStream :: [WithPos TToken]
  }

instance Stream TStream where
  type Token TStream = WithPos TToken
  type Tokens TStream = [WithPos TToken]

  tokenToChunk Proxy x = [x]
  tokensToChunk Proxy xs = xs
  chunkToTokens Proxy = id
  chunkLength Proxy = length
  chunkEmpty Proxy = null
  take1_ (TStream _ []) = Nothing
  take1_ (TStream str (t : ts)) =
    Just
      ( t,
        TStream (drop (tokensLength pxy (t :| [])) str) ts
      )
  takeN_ n (TStream str s)
    | n <= 0 = Just ([], TStream str s)
    | null s = Nothing
    | otherwise =
        let (x, s') = splitAt n s
         in case NE.nonEmpty x of
              Nothing -> Just (x, TStream str s')
              Just nex -> Just (x, TStream (drop (tokensLength pxy nex) str) s')
  takeWhile_ f (TStream str s) =
    let (x, s') = DL.span f s
     in case NE.nonEmpty x of
          Nothing -> (x, TStream str s')
          Just nex -> (x, TStream (drop (tokensLength pxy nex) str) s')

instance VisualStream TStream where
  showTokens Proxy =
    DL.intercalate " "
      . NE.toList
      . fmap (showTToken . tokenVal)
  tokensLength Proxy xs = sum (tokenLength <$> xs)

instance TraversableStream TStream where
  reachOffset o PosState {..} =
    ( Just (prefix ++ restOfLine),
      PosState
        { pstateInput =
            TStream
              { tStreamInput = postStr,
                unTStream = post
              },
          pstateOffset = max pstateOffset o,
          pstateSourcePos = newSourcePos,
          pstateTabWidth = pstateTabWidth,
          pstateLinePrefix = prefix
        }
    )
    where
      prefix =
        if sameLine
          then pstateLinePrefix ++ preLine
          else preLine
      sameLine = sourceLine newSourcePos == sourceLine pstateSourcePos
      newSourcePos =
        case post of
          [] -> case unTStream pstateInput of
            [] -> pstateSourcePos
            xs -> endPos (last xs)
          (x : _) -> startPos x
      (pre, post) = splitAt (o - pstateOffset) (unTStream pstateInput)
      (preStr, postStr) = splitAt tokensConsumed (tStreamInput pstateInput)
      preLine = reverse . takeWhile (/= '\n') . reverse $ preStr
      tokensConsumed =
        case NE.nonEmpty pre of
          Nothing -> 0
          Just nePre -> tokensLength pxy nePre
      restOfLine = takeWhile (/= '\n') postStr

pxy :: Proxy TStream
pxy = Proxy

showTToken :: TToken -> String
showTToken = \case
  TNull -> "null"
  (TBool v) -> (if v then "true" else "false")
  (TNumber v) -> show v
  (TString v) -> "\"" <> v <> "\""
  (TIdentifier v) -> v
  TEqual -> "="
  TModule -> "module"
  TImport -> "import"
  TExport -> "export"
  TDefine -> "define"
  TLParenthesis -> "("
  TRParenthesis -> ")"
  TLBracket -> "["
  TRBracket -> "]"
  TLBrace -> "{"
  TRBrace -> "}"
  TArrow -> "->"
  TComma -> ","
  TColon -> ":"
  TBackslash -> "\\"

type Lexer = Parsec Void String

mkTStream :: String -> String -> Either (ParseErrorBundle String Void) TStream
mkTStream fileName src = TStream src . filter notComment <$> parse tTokens fileName src
  where
    notComment (WithPos _ _ _ TComment) = False
    notComment _ = True

tTokens :: Lexer [WithPos TToken]
tTokens = many tToken <* eof

tToken :: Lexer (WithPos TToken)
tToken =
  lexeme $
    withPos $
      choice
        [ TComment <$ (L.skipLineComment "//" <?> "line comment"),
          TComment <$ (L.skipBlockComment "/*" "*/" <?> "block comment"),
          TLParenthesis <$ char '(' <?> "(",
          TRParenthesis <$ char ')' <?> ")",
          TLBracket <$ char '[' <?> "[",
          TRBracket <$ char ']' <?> "]",
          TLBrace <$ char '{' <?> "{",
          TRBrace <$ char '}' <?> "}",
          TArrow <$ string "->" <?> "->",
          TComma <$ char ',' <?> ",",
          TColon <$ char ':' <?> ":",
          TBackslash <$ char '\\' <?> "\\",
          TNull <$ string "null" <?> "null",
          TBool True <$ string "true" <?> "bool",
          TBool False <$ string "false" <?> "bool",
          TEqual <$ char '=' <?> "=",
          TModule <$ string "module" <?> "module",
          TImport <$ string "import" <?> "import",
          TExport <$ string "export" <?> "export",
          TDefine <$ string "define" <?> "define",
          TNumber <$> L.signed sc (toRealFloat <$> L.scientific) <?> "number",
          TString <$> (char '"' *> manyTill L.charLiteral (char '"') <?> "string"),
          TIdentifier <$> ((:) <$> (lowerChar <|> upperChar <|> char '$') <*> many (alphaNumChar <|> char '$') <?> "identifier")
        ]

lexeme :: Lexer a -> Lexer a
lexeme = L.lexeme sc

sc :: Lexer ()
sc = L.space space1 empty empty

withPos :: Lexer a -> Lexer (WithPos a)
withPos lexer = do
  start@(SourcePos _ _ c1) <- getSourcePos
  x <- lexer
  end@(SourcePos _ _ c2) <- getSourcePos
  pure $ WithPos start end (unPos c2 - unPos c1) x
