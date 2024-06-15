module Language.Daicker.Parser where

import Control.Monad (void)
import Control.Monad.Combinators.Expr (Operator (..), makeExprParser)
import Data.Functor.Identity (Identity)
import Data.Scientific (toRealFloat)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Void (Void)
import GHC.Conc (par)
import Language.Daicker.AST
import Language.Daicker.Span as S
import Language.LSP.Protocol.Lens (HasIdentifier (identifier))
import Language.LSP.Protocol.Types
import Text.Megaparsec
import Text.Megaparsec.Byte.Lexer (float)
import Text.Megaparsec.Char
  ( alphaNumChar,
    char,
    lowerChar,
    space1,
    string,
    symbolChar,
    upperChar,
  )
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void String

syntaxCheck :: String -> String -> [ParseErrorBundle String Void]
syntaxCheck fileName src = do
  case parse pModule fileName src of
    Right _ -> []
    Left e -> [e]

lexSemanticTokens :: String -> String -> Either Text [SemanticTokenAbsolute]
lexSemanticTokens fileName src =
  case parse tSemanticTokenTypes fileName src of
    Left e -> Left $ T.pack $ errorBundlePretty e
    Right ts -> Right $ makeSemanticTokenAbsolutes (SourcePos fileName (mkPos 1) (mkPos 1)) ts
  where
    makeSemanticTokenAbsolutes :: SourcePos -> [(SemanticTokenTypes, Span)] -> [SemanticTokenAbsolute]
    makeSemanticTokenAbsolutes _ [] = []
    makeSemanticTokenAbsolutes p0 (t@(_, Span _ p1) : ts) = makeSemanticTokenAbsolute p0 t : makeSemanticTokenAbsolutes p1 ts
    makeSemanticTokenAbsolute :: SourcePos -> (SemanticTokenTypes, Span) -> SemanticTokenAbsolute
    makeSemanticTokenAbsolute
      (SourcePos _ l0 c0)
      (t, Span (SourcePos _ l1 c1) (SourcePos _ l2 c2)) =
        SemanticTokenAbsolute
          (fromIntegral $ unPos l1 - 1)
          (fromIntegral $ unPos c1 - 1)
          (fromIntegral $ unPos c2 - unPos c1)
          t
          []
    tSemanticTokenTypes :: Parser [(SemanticTokenTypes, Span)]
    tSemanticTokenTypes =
      many $
        choice
          [ (,) SemanticTokenTypes_Keyword <$> fmap S.span tNull,
            (,) SemanticTokenTypes_Keyword <$> fmap S.span tBool,
            (,) SemanticTokenTypes_Number <$> fmap S.span tNumber,
            (,) SemanticTokenTypes_String <$> fmap S.span tString,
            (,) SemanticTokenTypes_Variable <$> fmap S.span tIdentifier,
            (,) SemanticTokenTypes_Operator <$> fmap S.span tEqual,
            (,) SemanticTokenTypes_Keyword <$> fmap S.span tModule,
            (,) SemanticTokenTypes_Keyword <$> fmap S.span tImport,
            (,) SemanticTokenTypes_Keyword <$> fmap S.span tExport,
            (,) SemanticTokenTypes_Keyword <$> fmap S.span tDefine,
            (,) SemanticTokenTypes_Operator <$> fmap S.span tLParenthesis,
            (,) SemanticTokenTypes_Operator <$> fmap S.span tRParenthesis,
            (,) SemanticTokenTypes_Operator <$> fmap S.span tLBracket,
            (,) SemanticTokenTypes_Operator <$> fmap S.span tRBracket,
            (,) SemanticTokenTypes_Operator <$> fmap S.span tLBrace,
            (,) SemanticTokenTypes_Operator <$> fmap S.span tRBrace,
            (,) SemanticTokenTypes_Operator <$> fmap S.span tArrow,
            (,) SemanticTokenTypes_Operator <$> fmap S.span tComma,
            (,) SemanticTokenTypes_Operator <$> fmap S.span tColon,
            (,) SemanticTokenTypes_Comment <$> fmap S.span (spanned tLineComment),
            (,) SemanticTokenTypes_Comment <$> fmap S.span (spanned tBlockComment)
          ]

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
  (_, s) <- tDefine
  i <- pIdentifier
  tEqual
  v <- pApp
  return $ Define i v (s S.<> S.span v)

pApp :: Parser Value
pApp = do
  img <- optional $ between tLBracket tRBracket pIdentifier
  (vs, s) <- spanned $ some pValue
  case img of
    Nothing -> return $ VApp Nothing vs s
    Just img -> return $ VApp (Just img) vs (S.span img S.<> s)

pValue :: Parser Value
pValue =
  choice
    [ pNull,
      pBool,
      pNumber,
      pString,
      pArray,
      pObject,
      pRef,
      pFunc,
      pApp'
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

pArray :: Parser Value
pArray = do
  (vs, s) <- spanned $ between tLBracket tRBracket (pValue `sepBy` tComma)
  return $ VArray vs s

pObject :: Parser Value
pObject = do
  (obj, s) <- spanned $ between tLBrace tRBrace (pair `sepBy` tComma)
  return $ VObject obj s
  where
    pair :: Parser (VKey, Value)
    pair = do
      (t, s) <- tString
      tColon
      v <- pValue
      return (Identifier t s, v)

pRef :: Parser Value
pRef = do
  i <- pIdentifier
  return $ VRef i (S.span i)

pApp' :: Parser Value
pApp' = do
  (VApp c vs _, s) <- spanned $ between tLParenthesis tRParenthesis pApp
  return $ VApp c vs s

pFunc :: Parser Value
pFunc = do
  (_, s) <- tBackslash
  args <- spanned $ many pIdentifier
  tArrow
  v <- pValue
  return $ VFun (fst args) v (s S.<> S.span v)

pIdentifier :: Parser Identifier
pIdentifier = do
  (t, s) <- tIdentifier
  return $ Identifier t s

tNull :: Parser ((), Span)
tNull = lexeme $ spanned $ void (string "null" <?> "null")

tBool :: Parser (Bool, Span)
tBool =
  lexeme $
    spanned
      ( choice
          [ True <$ string "true",
            False <$ string "false"
          ]
          <?> "bool"
      )

tNumber :: Parser (Double, Span)
tNumber = lexeme $ spanned (L.signed sc (toRealFloat <$> L.scientific) <?> "number")

tString :: Parser (String, Span)
tString = lexeme $ spanned (char '"' *> manyTill L.charLiteral (char '"') <?> "string")

tIdentifier :: Parser (String, Span)
tIdentifier = try $ do
  id <- lexeme $ spanned ((:) <$> (lowerChar <|> upperChar) <*> many alphaNumChar <?> "identifier")
  if fst id `elem` ["module", "import", "export", "define"]
    then fail $ "Keyword " ++ fst id ++ " cannot be an identifier"
    else return id

tEqual :: Parser ((), Span)
tEqual = lexeme $ spanned $ void (char '=' <?> "=")

tModule :: Parser ((), Span)
tModule = lexeme $ spanned $ void (string "module" <?> "module")

tImport :: Parser ((), Span)
tImport = lexeme $ spanned $ void (string "import" <?> "import")

tExport :: Parser ((), Span)
tExport = lexeme $ spanned $ void (string "export" <?> "export")

tDefine :: Parser ((), Span)
tDefine = lexeme $ spanned $ void (string "define" <?> "define")

-- | Left Parenthesis Token "("
tLParenthesis :: Parser ((), Span)
tLParenthesis = lexeme $ spanned $ void (char '(' <?> "(")

-- | Right Parenthesis Token ")"
tRParenthesis :: Parser ((), Span)
tRParenthesis = lexeme $ spanned $ void (char ')' <?> ")")

-- | Left Bracket Token "["
tLBracket :: Parser ((), Span)
tLBracket = lexeme $ spanned $ void (char '[' <?> "[")

-- | Right Bracket Token "]"
tRBracket :: Parser ((), Span)
tRBracket = lexeme $ spanned $ void (char ']' <?> "]")

-- | Left Brace Token "{"
tLBrace :: Parser ((), Span)
tLBrace = lexeme $ spanned $ void (char '{' <?> "{")

-- | Right Brace Token "}"
tRBrace :: Parser ((), Span)
tRBrace = lexeme $ spanned $ void (char '}' <?> "}")

-- | Right Arrow Token "->"
tArrow :: Parser ((), Span)
tArrow = lexeme $ spanned $ void (string "->" <?> "->")

tComma :: Parser ((), Span)
tComma = lexeme $ spanned $ void (char ',' <?> ",")

tColon :: Parser ((), Span)
tColon = lexeme $ spanned $ void (char ':' <?> ":")

tBackslash :: Parser ((), Span)
tBackslash = lexeme $ spanned $ void (char '\\' <?> "\\")

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

sc :: Parser ()
sc = L.space space1 tLineComment tBlockComment

tLineComment :: ParsecT Void String Identity ()
tLineComment = L.skipLineComment "//"

tBlockComment :: ParsecT Void String Identity ()
tBlockComment = L.skipBlockComment "/*" "*/"

spanned :: Parser a -> Parser (a, Span)
spanned parser = do
  start <- getSourcePos
  x <- parser
  end <- getSourcePos
  pure (x, Span start end)
