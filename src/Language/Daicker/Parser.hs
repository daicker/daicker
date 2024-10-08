module Language.Daicker.Parser where

import Control.Comonad.Cofree
import Control.Monad (void)
import Control.Monad.Combinators.Expr (Operator (..), makeExprParser)
import Data.Functor.Identity (Identity)
import Data.List.NonEmpty (NonEmpty (..), some1)
import Data.Maybe (fromJust, fromMaybe, isJust)
import Data.Scientific (toRealFloat)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import Data.Void (Void)
import GHC.Conc (par)
import Language.Daicker.AST hiding (Type, Type' (..))
import qualified Language.Daicker.AST as AST
import Language.Daicker.Error (StaticError (StaticError), fromParseErrorBundle)
import Language.Daicker.Lexer
import Language.Daicker.Span (Span (..), WithSpan (..), union)
import qualified Language.Daicker.Span as S
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
import Text.ParserCombinators.ReadP (many1)

type Parser = Parsec Void TStream

parseModule :: String -> TStream -> Either [StaticError] (Module Span)
parseModule fileName stream = case parse pModule fileName stream of
  Right m -> pure m
  Left e -> Left [fromParseErrorBundle e]

pModule :: Parser (Module Span)
pModule = do
  (WithSpan imports s1) <- spanned $ many pImport
  export <- optional pExport
  statements <- many pStatement
  (WithSpan _ s2) <- spanned eof
  return $
    (s1 `union` s2)
      :< Module
        imports
        export
        statements

pStatement :: Parser (NamedStatement Span)
pStatement =
  choice
    [ pDataOrDataTypeStatement,
      pExprOrExprTypeStatement,
      pTypeStatement
    ]

pImport :: Parser (Import Span)
pImport = do
  (WithSpan _ s1) <- pToken TImport
  s2 :< i <- pImport'
  return $ (s1 `union` s2) :< i
  where
    pImport' :: Parser (Import Span)
    pImport' =
      choice
        [ do
            (WithSpan is s1) <-
              spanned $
                between
                  (pToken TLBrace)
                  (pToken TRBrace)
                  ((pIdentifier <|> pTypeIdentifier) `sepBy` pToken TComma)
            pToken TFrom
            u@(s2 :< _) <- pUrl
            pure $ s1 `union` s2 :< PartialImport is u,
          do
            (WithSpan _ s1) <- pToken TMul
            pToken TFrom
            u@(s2 :< _) <- pUrl
            pure $ s1 `union` s2 :< WildImport u
        ]

pUrl :: Parser (URL Span)
pUrl = token test Set.empty <?> "url"
  where
    test (WithSpan (TString t) s) = Just $ s :< LocalFile t
    test _ = Nothing

pExport :: Parser (Export Span)
pExport = do
  (WithSpan _ s1) <- pToken TExport
  (WithSpan is s2) <- spanned $ some (pIdentifier <|> pTypeIdentifier)
  return $ (s1 `union` s2) :< Export is

pExprOrExprTypeStatement :: Parser (NamedStatement Span)
pExprOrExprTypeStatement = do
  i@(s :< _) <- pIdentifier
  isTypeDefine <- optional $ pToken T2Colons
  case isTypeDefine of
    Just _ -> do
      t <- pType
      pToken TSemicolon
      pure $
        (s `union` S.span t)
          :< NamedStatement i (S.span t :< SExprType (S.span t :< ExprType t))
    Nothing -> do
      params <- many pPatternMatchAssign
      extends <- optional $ pToken T3Dots
      pToken TAssign
      v <- pExpr
      pToken TSemicolon
      case params of
        [] ->
          return $
            (s `union` S.span v)
              :< NamedStatement i ((s `union` S.span v) :< SExpr v)
        params ->
          return $
            (s `union` S.span v)
              :< NamedStatement
                i
                ( (s `union` S.span v)
                    :< SExpr ((S.span (head params) `union` S.span v) :< EFun params v (isJust extends))
                )

pTypeStatement :: Parser (NamedStatement Span)
pTypeStatement = do
  (WithSpan _ s) <- pToken TType
  i <- pTypeIdentifier
  pToken TAssign
  t <- pType
  pure $ (s `union` S.span t) :< NamedStatement i ((s `union` S.span t) :< SType t)

pDataOrDataTypeStatement :: Parser (NamedStatement Span)
pDataOrDataTypeStatement = do
  (WithSpan _ s) <- pToken TData
  i <- pIdentifier
  isTypeDefine <- optional $ pToken T2Colons
  case isTypeDefine of
    Just _ -> do
      t <- pType
      pure $
        (s `union` S.span t)
          :< NamedStatement i (S.span t :< SDataType (S.span t :< DataType t))
    Nothing -> do
      pToken TAssign
      d <- pData
      pure $ (s `union` S.span i) :< NamedStatement i ((s `union` S.span i) :< SData d)

pData :: Parser (Data Span)
pData = pLocalState

pLocalState :: Parser (Data Span)
pLocalState = do
  (WithSpan _ s) <- pToken TState
  name <- pIdentifier
  pure $ (s `union` S.span name) :< LocalState name

pPatternMatchAssign :: Parser (PatternMatchAssign Span)
pPatternMatchAssign =
  choice
    [ anyValuePattern,
      arrayPattern,
      objectPattern
    ]
  where
    anyValuePattern = do
      i <- pIdentifier
      return $ S.span i :< PMAAnyValue i
    arrayPattern = do
      (WithSpan pmas s) <-
        spanned $
          between
            (pToken TLBracket)
            (pToken TRBracket)
            (pPatternMatchAssign `sepBy` pToken TComma)
      return $ s :< PMAArray pmas
    objectPattern = do
      (WithSpan pmas s) <-
        spanned $
          between
            (pToken TLBrace)
            (pToken TRBrace)
            (choice [pair, pair'] `sepBy` pToken TComma)
      return $ s :< PMAObject pmas
    pair = do
      s :< (EString t) <- pString
      pToken TColon
      v <- pPatternMatchAssign
      return (s :< Identifier t, v)
    pair' = do
      i <- pIdentifier
      return (i, S.span i :< PMAAnyValue i)

termOperatorTable :: [[Operator Parser (Expr Span)]]
termOperatorTable =
  [ [ binary TMul,
      binary TDiv
    ],
    [ binary TAdd,
      binary TSub
    ],
    [ binary TGTE,
      binary TGT,
      binary TLTE,
      binary TLT,
      binary TEQ,
      binary TNEQ
    ],
    [ prefix TNot
    ],
    [ binary TAnd,
      binary TOr
    ]
  ]

exprOperatorTable :: [[Operator Parser (Expr Span)]]
exprOperatorTable =
  [ [ binary TRight
    ]
  ]

binary :: TToken -> Operator Parser (Expr Span)
binary token = InfixL (f <$> pToken token)
  where
    f :: WithSpan TToken -> Expr Span -> Expr Span -> Expr Span
    f (WithSpan op s) a b =
      (S.span a `union` S.span b)
        :< EApp
          Nothing
          (s :< ERef (s :< Identifier (showTToken op)))
          [(a, False), (b, False)]

prefix :: TToken -> Operator Parser (Expr Span)
prefix token = Prefix (f <$> pToken token)
  where
    f :: WithSpan TToken -> Expr Span -> Expr Span
    f (WithSpan op s) a =
      (s `union` S.span a)
        :< EApp
          Nothing
          (s :< ERef (s :< Identifier (showTToken op)))
          [(a, False)]

postfix :: TToken -> Operator Parser (Expr Span)
postfix token = Postfix (f <$> pToken token)
  where
    f :: WithSpan TToken -> Expr Span -> Expr Span
    f (WithSpan op s) a =
      (s `union` S.span a)
        :< EApp
          Nothing
          (s :< ERef (s :< Identifier (showTToken op)))
          [(a, False)]

pType :: Parser (AST.Type Span)
pType = do
  args@(a1 : _) <- some pTypeTerm
  hasExtends <- isJust <$> optional (pToken T3Dots)
  hasArrow <- isJust <$> optional (pToken TArrow)
  if hasArrow
    then do
      v <- pType
      return $ (S.span (head args) `union` S.span v) :< AST.TFun args v hasExtends
    else
      if hasExtends
        then
          fail "'...' (variable length arguments) are only available for function types."
        else
          if length args == 1
            then pure a1
            else fail "Listing multiple types is available for functions."

pTypeTerm :: Parser (AST.Type Span)
pTypeTerm =
  choice
    [ pTVoid,
      pTNull,
      pTBool,
      pTNumber,
      pTString,
      pTArrayOrTuple,
      pTObject,
      pTRef,
      pToken TLParenthesis *> pType <* pToken TRParenthesis
    ]

pTRef :: Parser (AST.Type Span)
pTRef = do
  i <- pTypeIdentifier
  pure $ S.span i :< AST.TRef i

pTObject :: Parser (AST.Type Span)
pTObject = do
  (WithSpan obj s) <-
    spanned $
      between
        (pToken TLBrace)
        (pToken TRBrace)
        (pair `sepBy` pToken TComma)
  return $ s :< AST.TObject obj
  where
    pair :: Parser (EKey Span, AST.Type Span)
    pair = do
      s :< (EString k) <- pString
      pToken TColon
      t <- pType
      return (s :< Identifier k, t)

pTArrayOrTuple :: Parser (AST.Type Span)
pTArrayOrTuple = do
  (WithSpan ts s) <-
    spanned $
      between
        (pToken TLBracket)
        (pToken TRBracket)
        (pType `sepBy` pToken TComma)
  case ts of
    [] -> fail "type"
    [t] -> return $ s :< AST.TArray t
    _ -> return $ s :< AST.TTuple ts

pTString :: Parser (AST.Type Span)
pTString = token test Set.empty <?> "string type"
  where
    test (WithSpan (TIdentifier "string") s) = Just $ s :< AST.TString
    test _ = Nothing

pTNumber :: Parser (AST.Type Span)
pTNumber = token test Set.empty <?> "number type"
  where
    test (WithSpan (TIdentifier "number") s) = Just $ s :< AST.TNumber
    test _ = Nothing

pTBool :: Parser (AST.Type Span)
pTBool = token test Set.empty <?> "bool type"
  where
    test (WithSpan (TIdentifier "bool") s) = Just $ s :< AST.TBool
    test _ = Nothing

pTNull :: Parser (AST.Type Span)
pTNull = token test Set.empty <?> "null type"
  where
    test (WithSpan TNull s) = Just $ s :< AST.TNull
    test _ = Nothing

pTVoid :: Parser (AST.Type Span)
pTVoid = token test Set.empty <?> "void type"
  where
    test (WithSpan (TIdentifier "void") s) = Just $ s :< AST.TVoid
    test _ = Nothing

pExpr :: Parser (Expr Span)
pExpr = makeExprParser pExpr' exprOperatorTable <?> "expr"
  where
    pExpr' = do
      img <- optional pImage
      terms <- some $ do
        term <- pTerm <?> "expr"
        expand <- optional $ pToken T3Dots
        pure (term, isJust expand)
      case img of
        Nothing -> do
          case terms of
            [(e, _)] -> pure e
            (f, _) : es -> pure $ S.span f `union` S.span (fst $ last es) :< EApp Nothing f es
        Just (s :< Identifier i) -> do
          case terms of
            [(e, _)] -> pure e
            (f, _) : es -> pure $ s `union` S.span (fst $ last es) :< EApp (Just (s :< Identifier i)) f es

pTerm :: Parser (Expr Span)
pTerm = makeExprParser pValue termOperatorTable <?> "term"

pValue :: Parser (Expr Span)
pValue =
  choice
    [ pNull,
      pBool,
      pNumber,
      pString,
      pArray,
      pObject,
      pAccess,
      pRef,
      pFunc,
      pAssign,
      pExpr'
    ]

pNull :: Parser (Expr Span)
pNull = token test Set.empty <?> "null"
  where
    test (WithSpan TNull s) = Just $ s :< ENull
    test _ = Nothing

pBool :: Parser (Expr Span)
pBool = token test Set.empty <?> "bool"
  where
    test (WithSpan (TBool t) s) = Just $ s :< EBool t
    test _ = Nothing

pNumber :: Parser (Expr Span)
pNumber = token test Set.empty <?> "number"
  where
    test (WithSpan (TNumber t) s) = Just $ s :< ENumber t
    test _ = Nothing

pString :: Parser (Expr Span)
pString = token test Set.empty <?> "string"
  where
    test (WithSpan (TString t) s) = Just $ s :< EString t
    test _ = Nothing

pArray :: Parser (Expr Span)
pArray = do
  (WithSpan vs s) <-
    spanned $
      between
        (pToken TLBracket)
        (pToken TRBracket)
        (pExpr `sepBy` pToken TComma)
  return $ s :< EArray vs

pObject :: Parser (Expr Span)
pObject = do
  (WithSpan obj s) <-
    spanned $
      between
        (pToken TLBrace)
        (pToken TRBrace)
        (pair `sepBy` pToken TComma)
  return $ s :< EObject obj
  where
    pair :: Parser (EKey Span, Expr Span)
    pair = do
      s :< (EString t) <- pString
      pToken TColon
      v <- pExpr
      return (s :< Identifier t, v)

pRef :: Parser (Expr Span)
pRef = do
  i <- pIdentifier
  return $ S.span i :< ERef i

pAccess :: Parser (Expr Span)
pAccess = try $ do
  e <- pIdentifier
  pToken TDot
  i <- pIdentifier
  return $ S.span e `union` S.span i :< EProperty (S.span e :< ERef e) i

pExpr' :: Parser (Expr Span)
pExpr' = do
  (WithSpan (_ :< e) s) <-
    spanned $
      between
        (pToken TLParenthesis)
        (pToken TRParenthesis)
        pExpr
  pure (s :< e)

pFunc :: Parser (Expr Span)
pFunc = do
  (WithSpan _ s) <- pToken TBackslash
  args <- many pPatternMatchAssign
  extends <- optional $ pToken T3Dots
  pToken TArrow
  v <- pExpr
  return $ (s `union` S.span v) :< EFun args v (isJust extends)

pAssign :: Parser (Expr Span)
pAssign = do
  (WithSpan _ s) <- pToken TVar
  i <- pIdentifier
  pToken TAssign
  v <- pTerm
  return $ (s `union` S.span v) :< EAssign i v

pIdentifier :: Parser (Identifier Span)
pIdentifier = token test Set.empty <?> "identifier"
  where
    test (WithSpan (TIdentifier t) s) = Just $ s :< Identifier t
    test _ = Nothing

pTypeIdentifier :: Parser (Identifier Span)
pTypeIdentifier = token test Set.empty <?> "type identifier"
  where
    test (WithSpan (TTypeIdentifier t) s) = Just $ s :< Identifier t
    test _ = Nothing

pImage :: Parser (EImage Span)
pImage = token test Set.empty <?> "image"
  where
    test (WithSpan (TImage t) s) = Just $ s :< Identifier t
    test _ = Nothing

liftTToken :: TToken -> WithSpan TToken
liftTToken x = WithSpan x (Span pos pos)
  where
    pos = S.initialPos ""

pToken :: TToken -> Parser (WithSpan TToken)
pToken c = token test (Set.singleton . Tokens . nes . liftTToken $ c)
  where
    test (WithSpan x s) =
      if x == c
        then Just (WithSpan x s)
        else Nothing
    nes x = x :| []

spanned :: Parser a -> Parser (WithSpan a)
spanned parser = do
  start <- getSourcePos
  x <- parser
  end <- getSourcePos
  pure $ WithSpan x (Span (S.fromSourcePos start) (S.fromSourcePos end))
