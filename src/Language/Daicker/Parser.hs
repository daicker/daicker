{-# LANGUAGE OverloadedStrings #-}

module Language.Daicker.Parser where

import Control.Comonad.Cofree
import Control.Lens (op)
import Control.Monad (void)
import Control.Monad.Combinators.Expr (Operator (..), makeExprParser)
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
import Text.ParserCombinators.ReadP (many1)

data MetaNode = MetaNode
  { nodeSpan :: Span,
    nodeMetaTokens :: [MetaToken]
  }
  deriving (Show, Eq)

data Token a = Token
  { tokenMeta :: MetaToken,
    tokenValue :: a
  }
  deriving (Show, Eq)

data MetaToken = MetaToken
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
  | TKComment
  deriving (Show, Eq)

type Parser = Parsec Void Text

opTable :: [[Operator Parser (Expr MetaNode)]]
opTable =
  [ [ binary "*",
      binary "/"
    ],
    [ binary "+",
      binary "-"
    ]
  ]

binary :: Text -> Operator Parser (Expr MetaNode)
binary name = do
  InfixL (f <$> pOp name)
  where
    f :: Expr MetaNode -> Expr MetaNode -> Expr MetaNode -> Expr MetaNode
    f op@(op' :< _) a@(a' :< _) b@(b' :< _) =
      MetaNode (nodeSpan a' `union` nodeSpan b') (concatMap nodeMetaTokens [a', op', b'])
        :< EApp Nothing op [(a, False), (b, False)]

pOp :: Text -> Parser (Expr MetaNode)
pOp name = do
  Token m op <- tOp name
  pure $ singleTokenMetaNode m :< ERef (singleTokenMetaNode m :< Identifier op)

pExpr :: Parser (Expr MetaNode)
pExpr = makeExprParser pTerm opTable

pTerm :: Parser (Expr MetaNode)
pTerm = choice [pString, pNumber, pBool, pNull]

pArray :: Parser (Expr MetaNode)
pArray = do
  (Token s _) <- lexeme $ token TKOp $ char '['
  elements <- pExpr `sepBy` lexeme (char ',') -- TODO: Add trailing comma support and collect comma tokens.
  let elementTokens = concatMap (\(m :< _) -> nodeMetaTokens m) elements
  (Token e _) <- lexeme $ token TKOp $ char ']'
  pure $
    MetaNode
      (tokenSpan s `union` tokenSpan e)
      (concat [[s], elementTokens, [e]])
      :< EArray elements

pString :: Parser (Expr MetaNode)
pString = lexeme $ singleTokenNode <$> tString <*> pure EString

pNumber :: Parser (Expr MetaNode)
pNumber = lexeme $ singleTokenNode <$> tNumber <*> pure ENumber

pBool :: Parser (Expr MetaNode)
pBool = lexeme $ singleTokenNode <$> tBool <*> pure EBool

pNull :: Parser (Expr MetaNode)
pNull = lexeme $ (:< ENull) . singleTokenMetaNode . tokenMeta <$> tNull

singleTokenNode ::
  Token t ->
  (t -> f (Cofree f MetaNode)) ->
  Cofree f MetaNode
singleTokenNode (Token m v) f = singleTokenMetaNode m :< f v

singleTokenMetaNode :: MetaToken -> MetaNode
singleTokenMetaNode m = MetaNode (tokenSpan m) [m]

tExprIdentifier :: TokenKind -> Parser (Token String)
tExprIdentifier kind =
  token
    kind
    ( (:)
        <$> (lowerChar <|> char '_')
        <*> many (alphaNumChar <|> char '_' <|> char '-')
    )

tOp :: Text -> Parser (Token String)
tOp name = token TKOp (T.unpack <$> string name)

tString :: Parser (Token String)
tString = token TKString $ char '"' *> manyTill L.charLiteral (char '"')

tNumber :: Parser (Token Scientific)
tNumber = token TKNumber $ L.signed sc L.scientific

tBool :: Parser (Token Bool)
tBool = token TKKeyword $ (True <$ keyword "true") <|> (False <$ keyword "false")

tNull :: Parser (Token ())
tNull = token TKKeyword $ keyword "null"

token :: TokenKind -> Parser a -> Parser (Token a)
token kind p = do
  (s, v) <- spanned p
  pure $ Token (MetaToken kind s) v

keyword :: Text -> Parser ()
keyword k = try $ void (string k) <* notFollowedBy alphaNumChar

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

sc :: Parser ()
sc = L.space space1 lineComment blockComment

lineComment :: Parser ()
lineComment = L.skipLineComment "//"

blockComment :: Parser ()
blockComment = L.skipBlockComment "/*" "*/"

spanned :: Parser a -> Parser (Span, a)
spanned p = do
  start <- getSourcePos
  x <- p
  end <- getSourcePos
  pure (Span (S.fromSourcePos start) (S.fromSourcePos end), x)
