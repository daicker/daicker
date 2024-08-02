-- cf. https://markkarpov.com/tutorial/megaparsec.html#working-with-custom-input-streams
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}

module Language.Daicker.Lexer where

import Control.Monad (void, when)
import Data.Aeson (Value (Bool))
import qualified Data.List as DL
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import Data.Proxy
import Data.Scientific (Scientific, toRealFloat)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import Data.Void
import Language.Daicker.Error (StaticError (StaticError), fromParseErrorBundle)
import Language.Daicker.Span
  ( Span (Span, endPos, startPos),
    WithSpan (WithSpan, _value),
    fromSourcePos,
    toSourcePos,
  )
import qualified Language.Daicker.Span as S
import Text.Megaparsec
import Text.Megaparsec.Char (alphaNumChar, char, lowerChar, newline, space1, string, upperChar)
import qualified Text.Megaparsec.Char.Lexer as L

data TToken
  = TNull
  | TBool Bool
  | TNumber Scientific
  | TString String
  | TIdentifier String
  | TImage String
  | TAssign
  | TModule
  | TImport
  | TExport
  | TDefine
  | TType
  | TLParenthesis
  | TRParenthesis
  | TLBracket
  | TRBracket
  | TLBrace
  | TRBrace
  | TArrow
  | TComma
  | T3Dots
  | TDot
  | TColon
  | TNot
  | TOr
  | TLT
  | TLTE
  | TGT
  | TGTE
  | TEQ
  | TNEQ
  | TAnd
  | TAdd
  | TSub
  | TMul
  | TDiv
  | TBackslash
  | TSemicolon
  | TRight
  | TComment
  deriving (Eq, Ord, Show)

data TStream = TStream
  { tStreamInput :: Text, -- for showing offending lines
    unTStream :: [WithSpan TToken]
  }

instance Stream TStream where
  type Token TStream = WithSpan TToken
  type Tokens TStream = [WithSpan TToken]

  tokenToChunk Proxy x = [x]
  tokensToChunk Proxy xs = xs
  chunkToTokens Proxy = id
  chunkLength Proxy = length
  chunkEmpty Proxy = null
  take1_ (TStream _ []) = Nothing
  take1_ (TStream txt (t : ts)) =
    Just
      ( t,
        TStream (T.drop (tokensLength pxy (t :| [])) txt) ts
      )
  takeN_ n (TStream txt s)
    | n <= 0 = Just ([], TStream txt s)
    | null s = Nothing
    | otherwise =
        let (x, s') = splitAt n s
         in case NE.nonEmpty x of
              Nothing -> Just (x, TStream txt s')
              Just nex -> Just (x, TStream (T.drop (tokensLength pxy nex) txt) s')
  takeWhile_ f (TStream txt s) =
    let (x, s') = DL.span f s
     in case NE.nonEmpty x of
          Nothing -> (x, TStream txt s')
          Just nex -> (x, TStream (T.drop (tokensLength pxy nex) txt) s')

instance VisualStream TStream where
  showTokens Proxy =
    DL.intercalate " "
      . NE.toList
      . fmap (showTToken . _value)
  tokensLength Proxy xs = sum (S.length . S.span <$> xs)

instance TraversableStream TStream where
  reachOffset o PosState {..} =
    ( Just (T.unpack prefix ++ T.unpack restOfLine),
      PosState
        { pstateInput =
            TStream
              { tStreamInput = postTxt,
                unTStream = post
              },
          pstateOffset = max pstateOffset o,
          pstateSourcePos = newSourcePos,
          pstateTabWidth = pstateTabWidth,
          pstateLinePrefix = T.unpack prefix
        }
    )
    where
      prefix =
        if sameLine
          then T.pack pstateLinePrefix `T.append` preLine
          else preLine
      sameLine = sourceLine newSourcePos == sourceLine pstateSourcePos
      newSourcePos =
        case post of
          [] -> case unTStream pstateInput of
            [] -> pstateSourcePos
            xs -> toSourcePos (endPos (S.span (last xs)))
          (x : _) -> toSourcePos (startPos (S.span x))
      (pre, post) = splitAt (o - pstateOffset) (unTStream pstateInput)
      (preTxt, postTxt) = T.splitAt tokensConsumed (tStreamInput pstateInput)
      preLine = T.reverse . T.takeWhile (/= '\n') . T.reverse $ preTxt
      tokensConsumed =
        case NE.nonEmpty pre of
          Nothing -> 0
          Just nePre -> tokensLength pxy nePre
      restOfLine = T.takeWhile (/= '\n') postTxt

pxy :: Proxy TStream
pxy = Proxy

showTToken :: TToken -> String
showTToken = \case
  TNull -> "null"
  (TBool v) -> (if v then "true" else "false")
  (TNumber v) -> show v
  (TString v) -> "\"" <> v <> "\""
  (TIdentifier v) -> v
  (TImage v) -> "#" <> v
  TAssign -> "="
  TModule -> "module"
  TImport -> "import"
  TExport -> "export"
  TDefine -> "define"
  TType -> "type"
  TLParenthesis -> "("
  TRParenthesis -> ")"
  TLBracket -> "["
  TRBracket -> "]"
  TLBrace -> "{"
  TRBrace -> "}"
  TArrow -> "->"
  TComma -> ","
  T3Dots -> "..."
  TDot -> "."
  TColon -> ":"
  TNot -> "!"
  TOr -> "||"
  TAnd -> "&&"
  TLT -> "<"
  TLTE -> "<="
  TGT -> ">"
  TGTE -> ">="
  TEQ -> "=="
  TNEQ -> "!="
  TAdd -> "+"
  TSub -> "-"
  TMul -> "*"
  TDiv -> "/"
  TSemicolon -> ";"
  TRight -> "|>"
  TBackslash -> "\\"

type Lexer = Parsec Void Text

mkTStreamWithoutComment :: Text -> [WithSpan TToken] -> TStream
mkTStreamWithoutComment src tokens = TStream src $ filter notComment tokens
  where
    notComment (WithSpan TComment _) = False
    notComment _ = True

lexTokens :: String -> Text -> Either [StaticError] [WithSpan TToken]
lexTokens fileName src = case parse tTokens fileName src of
  (Left e) -> Left [fromParseErrorBundle e]
  (Right ts) -> pure ts

tTokens :: Lexer [WithSpan TToken]
tTokens = many tToken <* eof

tToken :: Lexer (WithSpan TToken)
tToken =
  lexeme $
    withSpan $
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
          T3Dots <$ string "..." <?> "...",
          TDot <$ char '.' <?> ".",
          TColon <$ char ':' <?> ":",
          TBackslash <$ char '\\' <?> "\\",
          TNull <$ string "null" <?> "null",
          TBool True <$ string "true" <?> "bool",
          TBool False <$ string "false" <?> "bool",
          TNot <$ char '!' <?> "!",
          TAnd <$ string "&&" <?> "&&",
          TOr <$ string "||" <?> "||",
          TLT <$ char '<' <?> ">",
          TLTE <$ string "<=" <?> "<=",
          TGT <$ char '>' <?> ">",
          TGTE <$ string ">=" <?> ">=",
          TEQ <$ string "==" <?> "==",
          TNEQ <$ string "!=" <?> "!=",
          TAdd <$ char '+' <?> "+",
          TSub <$ char '-' <?> "-",
          TMul <$ char '*' <?> "*",
          TDiv <$ char '/' <?> "/",
          TAssign <$ char '=' <?> "=",
          TSemicolon <$ char ';' <?> ";",
          TRight <$ string "|>" <?> "|>",
          TModule <$ string "module" <?> "module",
          TImage <$> ((:) <$> char '#' *> many (alphaNumChar <|> char '/' <|> char ':' <|> char '.' <|> char '-' <|> char '_') <?> "image"),
          TImport <$ string "import" <?> "import",
          TExport <$ string "export" <?> "export",
          TDefine <$ string "define" <?> "define",
          TType <$ string "type" <?> "type",
          TNumber <$> L.signed sc L.scientific <?> "number",
          TString <$> (char '"' *> manyTill L.charLiteral (char '"') <?> "string"),
          TIdentifier <$> ((:) <$> (lowerChar <|> upperChar <|> char '$' <|> char '_') <*> many (alphaNumChar <|> char '$' <|> char '_') <?> "identifier")
        ]

lexeme :: Lexer a -> Lexer a
lexeme = L.lexeme sc

sc :: Lexer ()
sc = L.space space1 empty empty

withSpan :: Lexer a -> Lexer (WithSpan a)
withSpan lexer = do
  start@(SourcePos _ _ c1) <- getSourcePos
  x <- lexer
  end@(SourcePos _ _ c2) <- getSourcePos
  pure $ WithSpan x $ Span (fromSourcePos start) (fromSourcePos end)
