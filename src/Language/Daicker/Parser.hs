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
import Data.Maybe (catMaybes, fromMaybe, isJust)
import Data.Scientific (Scientific, toRealFloat)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import Data.Void (Void)
import GHC.Conc (par)
import GHC.Event.TimeOut (TimeoutKey (TK))
import Language.Daicker.AST
import qualified Language.Daicker.AST as AST
import Language.Daicker.Error (StaticError (StaticError), fromParseErrorBundle)
import Language.Daicker.Span (Span (..), WithSpan (..), union)
import qualified Language.Daicker.Span as S
import Language.LSP.Protocol.Lens (HasCh (ch), HasIdentifier (identifier))
import Language.LSP.Protocol.Types
import Text.Megaparsec hiding (Token, parse, token)
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
  | TKVar
  | TKKeyword
  | TKNull
  | TKBool
  | TKNumber
  | TKString
  | TKSep
  | TKComment
  | TKParameter
  | TKTypeParameter
  | TKProperty
  | TKImage
  deriving (Show, Eq)

type Parser = StateT [Token] (Parsec Void Text)

parse :: Parser a -> FilePath -> Text -> Either [StaticError Span] (a, [Token])
parse parser filePath src = case runParser (runStateT parser []) filePath src of
  Left e -> Left [fromParseErrorBundle e]
  Right (a, tokens) -> Right (a, tokens)

pModule :: Parser (Module Span)
pModule = do
  sc
  p1 <- getSourcePos
  imports <- many pImport
  export <- optional pExport
  statements <- many pStatement
  eof
  p2 <- getSourcePos
  pure $
    Span (S.fromSourcePos p1) (S.fromSourcePos p2)
      :< Module
        imports
        export
        statements

pImport :: Parser (Import Span)
pImport = do
  (s1, _) <- lexeme $ spanned $ token TKKeyword (keyword "import")
  s2 :< i <- pImport'
  pure $ (s1 `union` s2) :< i
  where
    pImport' :: Parser (Import Span)
    pImport' =
      choice
        [ do
            (s1, is) <-
              spanned $
                between
                  (lexeme $ token TKSep $ char '{')
                  (lexeme $ token TKSep $ char '}')
                  ( ( tupleToCofree Identifier
                        <$> ( lexeme (spanned $ tTypeIdentifier TKTypeVar)
                                <|> lexeme (spanned $ tExprIdentifier TKVar)
                            )
                    )
                      `sepBy` lexeme (token TKSep (char ','))
                  )
            ns <-
              optional $
                lexeme
                  (token TKKeyword $ string' "as")
                  *> (tupleToCofree Identifier <$> lexeme (spanned (tExprIdentifier TKVar)))
            lexeme $ token TKKeyword $ string' "from"
            u@(s2 :< _) <- pUrl
            pure $ s1 `union` s2 :< PartialImport is ns u,
          do
            (s1, _) <- lexeme $ spanned $ token TKOp (char '*')
            ns <-
              optional $
                lexeme
                  (token TKKeyword $ string' "as")
                  *> (tupleToCofree Identifier <$> lexeme (spanned (tExprIdentifier TKVar)))
            lexeme $ token TKKeyword $ string' "from"
            u@(s2 :< _) <- pUrl
            pure $ s1 `union` s2 :< WildImport ns u
        ]

pUrl :: Parser (URL Span)
pUrl = lexeme $ tupleToCofree LocalFile <$> spanned tString
  where
    tString = token TKString $ char '"' *> manyTill L.charLiteral (char '"')

pExport :: Parser (Export Span)
pExport = do
  (s1, _) <- lexeme $ spanned $ token TKKeyword (keyword "export")
  (s2, is) <-
    spanned $
      between
        (lexeme $ token TKSep $ char '{')
        (lexeme $ token TKSep $ char '}')
        ( ( tupleToCofree Identifier
              <$> ( lexeme (spanned $ tTypeIdentifier TKTypeVar)
                      <|> lexeme (spanned $ tExprIdentifier TKVar)
                  )
          )
            `sepBy` lexeme (token TKSep (char ','))
        )
  pure $ (s1 `union` s2) :< Export is

pStatement :: Parser (Statement Span)
pStatement =
  choice
    [ pSType,
      pSExpr
    ]

pSType :: Parser (Statement Span)
pSType = do
  (s', t) <- spanned $ do
    lexeme $ token TKKeyword (keyword "type")
    name <- tupleToCofree Identifier <$> lexeme (spanned $ tTypeIdentifier TKTypeVar)
    params <-
      optional
        $ between
          (lexeme $ token TKSep $ char '[')
          (lexeme $ token TKSep $ char ']')
        $ (tupleToCofree Identifier <$> lexeme (spanned $ tTypeIdentifier TKTypeParameter)) `sepBy` lexeme (token TKSep (char ','))
    lexeme $ token TKSep $ char '='
    SType name (fromMaybe [] params) <$> pType
  pure $ s' :< t

pSExpr :: Parser (Statement Span)
pSExpr = do
  (s', e) <- spanned $ do
    name <- tupleToCofree Identifier <$> lexeme (spanned $ tExprIdentifier TKVar)
    params <-
      optional
        $ spanned
        $ between
          (lexeme $ token TKSep $ char '(')
          (lexeme $ token TKSep $ char ')')
        $ pParameter `sepBy` lexeme (token TKSep (char ','))
    exprType <- optional $ lexeme (token TKSep $ char ':') *> pType
    lexeme $ token TKSep $ char '='
    expr@(expr' :< _) <- pExpr
    case params of
      Just (params', params) -> pure $ SExpr name (params' `union` expr' :< ELambda params expr exprType)
      Nothing -> pure $ SExpr name expr
  pure $ s' :< e

pType :: Parser (Type Span)
pType = makeExprParser pTTerm typeOpTable

typeOpTable :: [[Operator Parser (Type Span)]]
typeOpTable =
  [ [ typeBinary "&",
      typeBinary "|"
    ]
  ]

typeBinary :: Text -> Operator Parser (Type Span)
typeBinary name = InfixL (f <$> pTypeOp name)
  where
    f :: Type Span -> Type Span -> Type Span -> Type Span
    f op a@(a' :< _) b@(b' :< _) =
      (a' `union` b')
        :< TParameterized op [a, b]

pTypeOp :: Text -> Parser (Type Span)
pTypeOp name = do
  (s, op) <- lexeme (spanned $ tOp name)
  pure $ s :< TVar (s :< Identifier op)
  where
    tOp name = token TKOp (T.unpack <$> string name)

pTTerm :: Parser (Type Span)
pTTerm = do
  v <- pTPrimary
  choice
    [ pTParameterized v,
      pure v
    ]
  where
    pTParameterized :: Type Span -> Parser (Type Span)
    pTParameterized f@(s1 :< _) = do
      (s2, ts) <-
        spanned $
          between
            (lexeme $ token TKSep $ char '[')
            (lexeme $ token TKSep $ char ']')
            (pType `sepBy` lexeme (token TKSep (char ',')))
      pure $ s1 `union` s2 :< TParameterized f ts

pTPrimary :: Parser (Type Span)
pTPrimary =
  choice
    [ pTAccessor,
      pTVar,
      pTArrayOrTuple,
      pTObject,
      pTStringLiteral,
      pTNumberLiteral,
      pTBoolLiteral,
      pTNullLiteral
    ]

pTVar :: Parser (Type Span)
pTVar =
  lexeme $
    tupleToCofree TVar
      <$> spanned
        ( tupleToCofree
            Identifier
            <$> lexeme (spanned (tTypeIdentifier TKTypeVar))
        )

pTAccessor :: Parser (Type Span)
pTAccessor = do
  (s, a) <- lexeme $ spanned $ do
    ns <- tupleToCofree Identifier <$> spanned (tExprIdentifier TKVar)
    _ <- token TKSep $ char '.'
    p <- tupleToCofree Identifier <$> spanned (tTypeIdentifier TKTypeVar)
    pure $ TAccessor ns p
  pure $ s :< a

-- array and tuple type sugar syntax
pTArrayOrTuple :: Parser (Type Span)
pTArrayOrTuple = do
  (s, ts) <-
    spanned $
      between
        (lexeme $ token TKSep $ char '[')
        (lexeme $ token TKSep $ char ']')
        (pType `sepBy` lexeme (token TKSep (char ',')))
  case ts of
    [] -> fail "type"
    [t] -> pure $ s :< TParameterized (s :< TVar (s :< Identifier "Array")) [t]
    _ -> pure $ s :< TParameterized (s :< TVar (s :< Identifier "Tuple")) ts

pTObject :: Parser (Type Span)
pTObject = do
  (s, obj) <-
    spanned $
      between
        (lexeme $ token TKSep $ char '{')
        (lexeme $ token TKSep $ char '}')
        (pair `sepBy` lexeme (token TKSep (char ',')))
  pure $ s :< TObject obj
  where
    pair :: Parser (Type Span, Type Span)
    pair = do
      key <- pType
      _ <- lexeme $ token TKSep $ char ':'
      val <- pType
      pure (key, val)

pTStringLiteral :: Parser (Type Span)
pTStringLiteral = lexeme $ tupleToCofree TStringLiteral <$> spanned tString
  where
    tString = token TKString $ char '"' *> manyTill L.charLiteral (char '"')

pTNumberLiteral :: Parser (Type Span)
pTNumberLiteral = lexeme $ tupleToCofree TNumberLiteral <$> spanned tNumber
  where
    tNumber = token TKNumber $ L.signed sc L.scientific

pTBoolLiteral :: Parser (Type Span)
pTBoolLiteral = lexeme $ tupleToCofree TBoolLiteral <$> spanned tBool
  where
    tBool = token TKBool $ (True <$ keyword "true") <|> (False <$ keyword "false")

pTNullLiteral :: Parser (Type Span)
pTNullLiteral = lexeme $ (:< TNullLiteral) . fst <$> spanned tNull
  where
    tNull = token TKNull $ keyword "null"

pExpr :: Parser (Expr Span)
pExpr = makeExprParser pTerm opTable

opTable :: [[Operator Parser (Expr Span)]]
opTable =
  [ [ unary "!"
    ],
    [ binary "*",
      binary "/"
    ],
    [ binary "+",
      binary "-"
    ],
    [ binary "==",
      binary "!=",
      binary "<",
      binary "<=",
      binary ">",
      binary ">="
    ],
    [ binary "&&",
      binary "||"
    ],
    [ binary "|>",
      binary ">>"
    ]
  ]

unary :: Text -> Operator Parser (Expr Span)
unary name = Prefix (f <$> pOp name)
  where
    f :: Expr Span -> Expr Span -> Expr Span
    f op@(op' :< _) v@(v' :< _) =
      (op' `union` v')
        :< ECall op [v' :< PositionedArgument v]

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
      key@(s2 :< _) <- tupleToCofree EString <$> spanned (tExprIdentifier TKProperty)
      pure $ s1 `union` s2 :< EAccessor v key
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
        paramType <- optional $ lexeme (token TKSep $ char ':') *> pType
        sc
        defaultExpr <- optional $ lexeme (token TKOp $ char '=') *> pExpr
        pure $ PositionedParameter name isRest isOptional paramType defaultExpr
      pure $ s :< p
    keywordParameter :: Parser (Parameter Span)
    keywordParameter = do
      (s, p) <- spanned $ do
        optional $ token TKOp $ char '-'
        name@(s1 :< _) <- tupleToCofree Identifier <$> spanned (tExprIdentifier TKVar)
        isOptional <- isJust <$> optional (token TKOp $ char '?')
        isRest <- isJust <$> optional (lexeme $ token TKOp $ string "...")
        paramType <- optional $ lexeme (token TKSep $ char ':') *> pType
        defaultExpr <- optional $ token TKOp $ char '=' *> pExpr
        pure $ KeywordParameter name isOptional isRest paramType defaultExpr
      pure $ s :< p

pPrimary :: Parser (Expr Span)
pPrimary =
  choice
    [ pLambda,
      pCommand,
      pImage,
      pObject,
      pArray,
      pString,
      pNumber,
      pBool,
      pNull,
      pVar
    ]

-- command sugar syntax
pCommand :: Parser (Expr Span)
pCommand = do
  (s, command) <- lexeme $ spanned $ do
    (name', name) <- lexeme $ spanned $ token TKVar $ ("$" ++) <$> (char '$' *> many (alphaNumChar <|> char '_'))
    image <-
      optional $
        spanned
          ( between
              (lexeme $ token TKSep $ char '[')
              (lexeme $ token TKSep $ char ']')
              pImage
          )
    (cmd', cmd) <- lexeme $ spanned $ token TKString $ manyTill L.charLiteral (string ";;")
    pure $
      ECall
        (name' :< EVar (name' :< Identifier name))
        ( catMaybes
            [ fmap (\(image', image) -> image' :< KeywordArgument (image' :< Identifier "image") image) image,
              Just $ cmd' :< PositionedArgument (cmd' :< EString cmd)
            ]
        )
  pure $ s :< command

pLambda :: Parser (Expr Span)
pLambda = do
  (s1, _) <- spanned $ lexeme $ token TKSep $ string' "\\"
  params <-
    between
      (lexeme $ token TKSep $ char '(')
      (lexeme $ token TKSep $ char ')')
      $ pParameter `sepBy` lexeme (token TKSep (char ','))
  exprType <- optional $ lexeme (token TKSep $ char ':') *> pType
  _ <- lexeme $ token TKSep $ string' "->"
  body@(s2 :< _) <- pExpr
  pure $ s1 `union` s2 :< ELambda params body exprType

pVar :: Parser (Expr Span)
pVar =
  lexeme $
    tupleToCofree EVar
      <$> spanned
        ( tupleToCofree
            Identifier
            <$> spanned (tExprIdentifier TKVar)
        )

pImage :: Parser (Expr Span)
pImage = do
  (s, image) <-
    lexeme $
      spanned $
        token TKImage $
          (:) <$> char '#' *> many (alphaNumChar <|> char '/' <|> char ':' <|> char '.' <|> char '-' <|> char '_')
  pure $ s :< EImage (s :< Identifier image)

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

tTypeIdentifier :: TokenKind -> Parser String
tTypeIdentifier kind =
  token
    kind
    ( (:)
        <$> (upperChar <|> char '_')
        <*> many (alphaNumChar <|> char '_')
    )

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
