module Language.Daicker.Parser where

import Text.Megaparsec
import qualified Text.Megaparsec.Char.Lexer as L
import Data.Void ( Void )
import Data.Scientific ( toRealFloat )
import Language.Daicker.AST
import Text.Megaparsec.Char
import Text.Megaparsec.Byte.Lexer (float)
import Language.Daicker.Span
import Control.Monad (void)
import GHC.Conc (par)

type Parser = Parsec Void String

pValue :: Parser Value
pValue = choice
  [ pNull
  , pBool
  , pNumber
  , pString
  , pRef
  ]

pNull :: Parser Value
pNull = do
  (_, span) <- spanned tNull
  return $ VNull span

pBool :: Parser Value
pBool = do
  (t, span) <- spanned tBool
  return $ VBool t span

pNumber :: Parser Value
pNumber = do
  (t, span) <- spanned tNumber
  return $ VNumber t span

pString :: Parser Value
pString = do
  (t, span) <- spanned tString
  return $ VString t span

pRef :: Parser Value
pRef = do
  (t, span) <- spanned tIdentifier
  return $ VRef (Identifier t span) span

tNull :: Parser ()
tNull = lexeme (void (string "null"))

tBool :: Parser Bool
tBool = lexeme (choice
  [ True <$ string "true"
  , False <$ string "false"
  ] <?> "bool")

tNumber :: Parser Double
tNumber = lexeme (L.signed sc (lexeme $ toRealFloat <$> L.scientific) <?> "number")

tString :: Parser String
tString = lexeme (char '"' *> manyTill L.charLiteral (char '"') <?> "string")

tIdentifier :: Parser String
tIdentifier = lexeme ((:) <$> (lowerChar <|> upperChar) <*> many alphaNumChar <?> "identifier")

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

sc :: Parser ()
sc = L.space space1 (L.skipLineComment "//") (L.skipBlockComment "/*" "*/")

spanned :: Parser a -> Parser (a, Span)
spanned parser = do
  start <- getSourcePos
  x <- parser
  end <- getSourcePos
  pure (x,  Span start end)
