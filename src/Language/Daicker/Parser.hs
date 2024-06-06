module Language.Daicker.Parser where

import Text.Megaparsec
import qualified Text.Megaparsec.Char.Lexer as L
import Data.Void ( Void )
import Data.Scientific ( toRealFloat )
import Language.Daicker.AST
import Text.Megaparsec.Char
import Text.Megaparsec.Byte.Lexer (float)
import Language.Daicker.Span as S
import Control.Monad (void)
import GHC.Conc (par)
import Language.LSP.Protocol.Lens (HasIdentifier(identifier))

type Parser = Parsec Void String

syntaxCheck :: String -> String -> [ParseErrorBundle String Void]
syntaxCheck fileName src = do
  case parse pModule fileName src of
    Right _ -> []
    Left e -> [e]

pModule :: Parser Module
pModule = Module <$> (tModule *> pIdentifier) <*> many pImport <*> many pExport <*> many pDefine <* eof

pImport :: Parser Import
pImport = do
  s <- tImport
  i <- pIdentifier
  return $ Import i (S.span s S.<> S.span i)

pExport :: Parser Export
pExport = do
  s <- tExport
  i <- pIdentifier
  return $ Export i (S.span s S.<> S.span i)

pDefine :: Parser Define
pDefine = do
  i <- pIdentifier
  tEqual
  v <- pValue
  return $ Define i v (S.span i S.<> S.span v)

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
  (_, span) <- tNull
  return $ VNull span

pBool :: Parser Value
pBool = do
  (t, s) <- tBool
  return $ VBool t s

pNumber :: Parser Value
pNumber = do
  (t, s) <- tNumber
  return $ VNumber t s

pString :: Parser Value
pString = do
  (t, s) <- tString
  return $ VString t s

pRef :: Parser Value
pRef = do
  i <- pIdentifier
  return $ VRef i (S.span i)

pIdentifier :: Parser Identifier
pIdentifier = do
  (t, s) <- tIdentifier
  return $ Identifier t s

tNull :: Parser ((), Span)
tNull = lexeme $ spanned $ void (string "null" <?> "null")

tBool :: Parser (Bool, Span)
tBool = lexeme $ spanned (choice
  [ True <$ string "true"
  , False <$ string "false"
  ] <?> "bool")

tNumber :: Parser (Double, Span)
tNumber = lexeme $ spanned (L.signed sc (lexeme $ toRealFloat <$> L.scientific) <?> "number")

tString :: Parser (String, Span)
tString = lexeme $ spanned (char '"' *> manyTill L.charLiteral (char '"') <?> "string")

tIdentifier :: Parser (String, Span)
tIdentifier = lexeme $ spanned ((:) <$> (lowerChar <|> upperChar) <*> many alphaNumChar <?> "identifier")

tEqual :: Parser ((), Span)
tEqual = lexeme $ spanned $ void (char '=' <?> "equal")

tModule :: Parser ((), Span)
tModule = lexeme $ spanned $ void (string "module" <?> "module")

tImport :: Parser ((), Span)
tImport = lexeme $ spanned $ void (string "import" <?> "import")

tExport :: Parser ((), Span)
tExport = lexeme $ spanned $ void (string "export" <?> "export")

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

sc :: Parser ()
sc = L.space space1 (L.skipLineComment "//") (L.skipBlockComment "/*" "*/")

spanned :: Parser a -> Parser (a, Span)
spanned parser = do
  start <- getSourcePos
  x <- parser
  end <- getSourcePos
  pure (x, Span start end)
