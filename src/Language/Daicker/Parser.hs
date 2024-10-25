{-# LANGUAGE OverloadedStrings #-}

module Language.Daicker.Parser where

import Control.Comonad.Cofree
import Control.Lens (op)
import Control.Monad (void)
import Control.Monad.Combinators.Expr (Operator (..), makeExprParser)
import Control.Monad.State (StateT)
import Control.Monad.State.Class (get, put)
import Control.Monad.State.Lazy (StateT (runStateT))
import Data.Functor.Identity (Identity)
import Data.List.NonEmpty (NonEmpty (..), some1)
import Data.Maybe (fromJust, fromMaybe, isJust)
import Data.Scientific (Scientific, toRealFloat)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import Data.Void (Void)
import GHC.Conc (par)
import GHC.Event.TimeOut (TimeoutKey (TK))
import Language.Daicker.AST hiding (Type, Type' (..))
import qualified Language.Daicker.AST as AST
import Language.Daicker.Error (StaticError (StaticError), fromParseErrorBundle)
import Language.Daicker.Span (Span (..), WithSpan (..), union)
import qualified Language.Daicker.Span as S
import Language.LSP.Protocol.Lens (HasCh (ch), HasIdentifier (identifier))
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
  | TKNull
  | TKBool
  | TKNumber
  | TKString
  | TKSep
  | TKComment
  | TKParameter
  deriving (Show, Eq)

type Parser = StateT [Token] (Parsec Void Text)

parse :: Parser a -> String -> Text -> Either (ParseErrorBundle Text Void) (a, [Token])
parse parser = runParser (runStateT parser [])

pExpr :: Parser (Expr Span)
pExpr = makeExprParser pTerm opTable

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
        :< ECall op [a' :< PositionedArgument a, b' :< PositionedArgument b]

pOp :: Text -> Parser (Expr Span)
pOp name = do
  (s, op) <- lexeme (spanned $ tOp name)
  pure $ s :< EVar (s :< Identifier op)
  where
    tOp name = token TKOp (T.unpack <$> string name)

pTerm :: Parser (Expr Span)
pTerm = do
  v <- pPrimary
  choice
    [ pCall v,
      pBracketAccessor v,
      pDotAccessor v,
      pure v
    ]
  where
    pCall :: Expr Span -> Parser (Expr Span)
    pCall f@(s1 :< _) = do
      (s2, es) <-
        spanned
          $ between
            (lexeme $ token TKSep $ char '(')
            (lexeme $ token TKSep $ char ')')
          $ pArgument `sepBy` lexeme (token TKSep (char ','))
      pure $ s1 `union` s2 :< ECall f es
    pDotAccessor :: Expr Span -> Parser (Expr Span)
    pDotAccessor v@(s1 :< _) = do
      _ <- lexeme $ token TKSep $ char '.'
      key@(s2 :< _) <- tupleToCofree Identifier <$> spanned (tExprIdentifier TKVar)
      pure $ s1 `union` s2 :< EAccessor v (s2 :< EVar key)
    pBracketAccessor :: Expr Span -> Parser (Expr Span)
    pBracketAccessor v@(s1 :< _) = do
      (s2, key) <-
        spanned $
          between
            (lexeme $ token TKSep $ char '[')
            (lexeme $ token TKSep $ char ']')
            pExpr
      pure $ s1 `union` s2 :< EAccessor v key

pArgument :: Parser (Argument Span)
pArgument =
  choice
    [ try keywordArgument,
      positionedArgument
    ]
  where
    positionedArgument :: Parser (Argument Span)
    positionedArgument = do
      (s1, value) <- spanned pExpr
      pure $ s1 :< PositionedArgument value
    keywordArgument :: Parser (Argument Span)
    keywordArgument = do
      key@(s1 :< _) <- tupleToCofree Identifier <$> lexeme (spanned (tExprIdentifier TKParameter))
      _ <- lexeme $ token TKSep $ char '='
      (s2, value) <- spanned pExpr
      pure $ s1 `union` s2 :< KeywordArgument key value

pParameter :: Parser (Parameter Span)
pParameter =
  choice
    [ positionedParameter,
      keywordParameter
    ]
  where
    positionedParameter :: Parser (Parameter Span)
    positionedParameter = do
      (s, p) <- spanned $ do
        name@(s1 :< _) <- tupleToCofree Identifier <$> spanned (tExprIdentifier TKParameter)
        isOptional <- isJust <$> optional (token TKOp $ char '?')
        isRest <- isJust <$> optional (lexeme $ token TKOp $ string "...")
        sc
        defaultExpr <- optional $ lexeme (token TKOp $ char '=') *> pExpr
        pure $ PositionedParameter name isRest isOptional Nothing defaultExpr
      pure $ s :< p
    keywordParameter :: Parser (Parameter Span)
    keywordParameter = do
      (s, p) <- spanned $ do
        optional $ token TKOp $ char '-'
        name@(s1 :< _) <- tupleToCofree Identifier <$> spanned (tExprIdentifier TKVar)
        isOptional <- isJust <$> optional (token TKOp $ char '?')
        isRest <- isJust <$> optional (token TKOp $ string "...")
        defaultExpr <- optional $ token TKOp $ char '=' *> pExpr
        pure $ KeywordParameter name isOptional isRest Nothing defaultExpr
      pure $ s :< p

pPrimary :: Parser (Expr Span)
pPrimary =
  choice
    [ pLambda,
      pObject,
      pArray,
      pString,
      pNumber,
      pBool,
      pNull,
      pVar
    ]

pLambda :: Parser (Expr Span)
pLambda = do
  (s1, _) <- spanned $ lexeme $ token TKSep $ string' "\\"
  params <-
    between
      (lexeme $ token TKSep $ char '(')
      (lexeme $ token TKSep $ char ')')
      $ pParameter `sepBy` lexeme (token TKSep (char ','))
  _ <- lexeme $ token TKSep $ string' "->"
  body@(s2 :< _) <- pExpr
  pure $ s1 `union` s2 :< ELambda params body

pVar :: Parser (Expr Span)
pVar =
  lexeme $
    tupleToCofree EVar
      <$> spanned
        ( tupleToCofree
            Identifier
            <$> spanned (tExprIdentifier TKVar)
        )

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
      _ <- lexeme $ token TKSep $ char ':'
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
  where
    tString = token TKString $ char '"' *> manyTill L.charLiteral (char '"')

pNumber :: Parser (Expr Span)
pNumber = lexeme $ tupleToCofree ENumber <$> spanned tNumber
  where
    tNumber = token TKNumber $ L.signed sc L.scientific

pBool :: Parser (Expr Span)
pBool = lexeme $ tupleToCofree EBool <$> spanned tBool
  where
    tBool = token TKBool $ (True <$ keyword "true") <|> (False <$ keyword "false")

pNull :: Parser (Expr Span)
pNull = lexeme $ (:< ENull) . fst <$> spanned tNull
  where
    tNull = token TKNull $ keyword "null"

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
lineComment = token TKComment $ L.skipLineComment "//"

blockComment :: Parser ()
blockComment = token TKComment $ L.skipBlockComment "/*" "*/"

spanned :: Parser a -> Parser (Span, a)
spanned p = do
  start <- length <$> get
  x <- p
  end <- length <$> get
  tokens <- take (end - start) . drop start <$> get
  pure (tokenSpan (head tokens) `union` tokenSpan (last tokens), x)
