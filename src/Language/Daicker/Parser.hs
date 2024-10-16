{-# LANGUAGE OverloadedStrings #-}

module Language.Daicker.Parser where

import Control.Comonad.Cofree
import Control.Lens (op)
import Control.Monad (void)
import Control.Monad.Combinators.Expr (Operator (..), makeExprParser)
import Control.Monad.State (StateT)
import Control.Monad.State.Class (get, put)
import Data.Functor.Identity (Identity)
import Data.List.NonEmpty (NonEmpty (..), some1)
import Data.Maybe (fromJust, fromMaybe, isJust)
import Data.Scientific (Scientific, toRealFloat)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import Data.Void (Void)
import GHC.Conc (par)
import Language.Daicker.AST hiding (Type, Type' (..))
import qualified Language.Daicker.AST as AST
import Language.Daicker.Error (StaticError (StaticError), fromParseErrorBundle)
import Language.Daicker.Span (Span (..), WithSpan (..), union)
import qualified Language.Daicker.Span as S
import Language.LSP.Protocol.Lens (HasIdentifier (identifier))
import Language.LSP.Protocol.Types
import Text.Megaparsec hiding (Token, token)
import Text.Megaparsec.Byte.Lexer (float)
import Text.Megaparsec.Char
  ( alphaNumChar,
    char,
    lowerChar,
    space1,
    string,
    string',
    symbolChar,
    upperChar,
  )
import qualified Text.Megaparsec.Char.Lexer as L

data Token = Token
  { tokenKind :: TokenKind,
    tokenSpan :: Span
  }
  deriving (Show, Eq)

data TokenKind
  = TKTypeVar
  | TKOp
  | TKFun
  | TKVar
  | TKKeyword
  | TKNumber
  | TKString
  | TKSep
  | TKComment
  deriving (Show, Eq)

type Parser = StateT [Token] (Parsec Void Text)

opTable :: [[Operator Parser (Expr Span)]]
opTable =
  [ [ binary "*",
      binary "/"
    ],
    [ binary "+",
      binary "-"
    ]
  ]

binary :: Text -> Operator Parser (Expr Span)
binary name = do
  InfixL (f <$> pOp name)
  where
    f :: Expr Span -> Expr Span -> Expr Span -> Expr Span
    f op a@(a' :< _) b@(b' :< _) =
      (a' `union` b')
        :< EApp Nothing op [(a, False), (b, False)]

pOp :: Text -> Parser (Expr Span)
pOp name = do
  (s, op) <- spanned $ tOp name
  pure $ s :< ERef (s :< Identifier op)

pExpr :: Parser (Expr Span)
pExpr = makeExprParser pTerm opTable

pTerm :: Parser (Expr Span)
pTerm =
  choice
    [ pObject,
      pArray,
      pString,
      pNumber,
      pBool,
      pNull
    ]

pVar :: Parser (Expr Span)
pVar = lexeme $ tupleToCofree EVar <$> spanned (tExprIdentifier TKVar)

pObject :: Parser (Expr Span)
pObject = do
  tupleToCofree EObject
    <$> spanned
      ( between
          (lexeme $ token TKSep $ char '{')
          (lexeme $ token TKSep $ char '}')
          (pPair `sepBy` lexeme (token TKSep (char ',')))
      )
  where
    pPair :: Parser (Expr Span, Expr Span)
    pPair = do
      key <- pExpr
      _ <- lexeme $ token TKOp $ char ':'
      value <- pExpr
      pure (key, value)

pArray :: Parser (Expr Span)
pArray =
  tupleToCofree EArray
    <$> spanned
      ( between
          (lexeme $ token TKSep $ char '[')
          (lexeme $ token TKSep $ char ']')
          (pExpr `sepBy` lexeme (token TKSep (char ','))) -- TODO: Add trailing comma support.
      )

pString :: Parser (Expr Span)
pString = lexeme $ tupleToCofree EString <$> spanned tString

pNumber :: Parser (Expr Span)
pNumber = lexeme $ tupleToCofree ENumber <$> spanned tNumber

pBool :: Parser (Expr Span)
pBool = lexeme $ tupleToCofree EBool <$> spanned tBool

pNull :: Parser (Expr Span)
pNull = lexeme $ (:< ENull) . fst <$> spanned tNull

tupleToCofree :: (t -> f (Cofree f a)) -> (a, t) -> Cofree f a
tupleToCofree f (s, v) = s :< f v

tExprIdentifier :: TokenKind -> Parser String
tExprIdentifier kind =
  token
    kind
    ( (:)
        <$> (lowerChar <|> char '_')
        <*> many (alphaNumChar <|> char '_' <|> char '-')
    )

tOp :: Text -> Parser String
tOp name = token TKOp (T.unpack <$> string name)

tString :: Parser String
tString = token TKString $ char '"' *> manyTill L.charLiteral (char '"')

tNumber :: Parser Scientific
tNumber = token TKNumber $ L.signed sc L.scientific

tBool :: Parser Bool
tBool = token TKKeyword $ (True <$ keyword "true") <|> (False <$ keyword "false")

tNull :: Parser ()
tNull = token TKKeyword $ keyword "null"

token :: TokenKind -> Parser a -> Parser a
token kind p = do
  (s, v) <- tokenSpanned p
  tokens <- get
  put $ tokens <> [Token kind s]
  pure v
  where
    tokenSpanned :: Parser a -> Parser (Span, a)
    tokenSpanned p = do
      start <- getSourcePos
      x <- p
      end <- getSourcePos
      pure (Span (S.fromSourcePos start) (S.fromSourcePos end), x)

keyword :: Text -> Parser ()
keyword k = try $ void (string k) <* notFollowedBy alphaNumChar

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

sc :: Parser ()
sc = L.space space1 lineComment blockComment

lineComment :: Parser ()
lineComment = L.skipLineComment "//"

blockComment :: Parser ()
blockComment = token TKComment $ L.skipBlockComment "/*" "*/"

spanned :: Parser a -> Parser (Span, a)
spanned p = do
  start <- length <$> get
  x <- p
  end <- length <$> get
  tokens <- take (end - start) . drop start <$> get
  pure (tokenSpan (head tokens) `union` tokenSpan (last tokens), x)
