module Language.Daicker.Parser where

import Control.Comonad.Cofree
import Control.Monad (void)
import Control.Monad.Combinators.Expr (Operator (..), makeExprParser)
import Data.Functor.Identity (Identity)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NEL
import Data.Scientific (toRealFloat)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import Data.Void (Void)
import GHC.Conc (par)
import Language.Daicker.AST
import Language.Daicker.Lexer
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

type Parser = Parsec Void TStream

syntaxCheck :: String -> String -> [(SourcePos, String)]
syntaxCheck fileName src =
  case mkTStream fileName src of
    Left e -> [(errorBundleSourcePos e, errorBundlePretty e)]
    Right ts ->
      case parse pModule fileName ts of
        Right m -> []
        Left e -> [(errorBundleSourcePos e, errorBundlePretty e)]

pModule :: Parser (Module Span)
pModule = Module <$> (pToken TModule *> pIdentifier) <*> many pImport <*> many pExport <*> many pDefine <* eof

pImport :: Parser (Import Span)
pImport = do
  s <- pToken TImport
  i <- pIdentifier
  return $ Import i (S.span s S.<> S.span i)

pExport :: Parser (Export Span)
pExport = do
  s <- pToken TExport
  i <- pIdentifier
  return $ Export i (S.span s S.<> S.span i)

pDefine :: Parser (Define Span)
pDefine = do
  (_, s) <- pToken TDefine
  i <- pIdentifier
  params <- many pIdentifier
  pToken TAssign
  v <- pExpr
  case params of
    [] -> return $ Define i v (s S.<> S.span v)
    _ -> return $ Define i ((S.span (head params) S.<> S.span v) :< EFun params v) (s S.<> S.span v)

operatorTable :: [[Operator Parser (Expr Span)]]
operatorTable =
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

binary :: TToken -> Operator Parser (Expr Span)
binary token = InfixL (f <$> pToken token)
  where
    f :: (TToken, Span) -> Expr Span -> Expr Span -> Expr Span
    f op a b =
      (S.span a S.<> S.span b)
        :< EApp
          Nothing
          [ S.span op :< ERef (Identifier (showTToken (fst op)) (S.span op)),
            a,
            b
          ]

prefix :: TToken -> Operator Parser (Expr Span)
prefix token = Prefix (f <$> pToken token)
  where
    f :: (TToken, Span) -> Expr Span -> Expr Span
    f op a =
      (S.span op S.<> S.span a)
        :< EApp
          Nothing
          [ S.span op :< ERef (Identifier (showTToken (fst op)) (S.span op)),
            a
          ]

pExpr :: Parser (Expr Span)
pExpr = do
  img <- optional $ spanned $ pToken THash *> pIdentifier
  case img of
    Nothing -> do
      terms <- some pTerm <?> "expr"
      case terms of
        [e] -> pure e
        es -> pure $ foldl1 (S.<>) (map S.span es) :< EApp Nothing es
    Just (Identifier i _, s) -> do
      terms <- some pTerm <?> "expr"
      pure $ (s S.<> foldl1 (S.<>) (map S.span terms)) :< EApp (Just (Identifier i s)) terms

pTerm :: Parser (Expr Span)
pTerm = makeExprParser pValue operatorTable <?> "term"

pValue :: Parser (Expr Span)
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
      pExpr'
    ]

pNull :: Parser (Expr Span)
pNull = token test Set.empty <?> "null"
  where
    test (WithPos s e _ TNull) = Just $ Span s e :< ENull
    test _ = Nothing

pBool :: Parser (Expr Span)
pBool = token test Set.empty <?> "bool"
  where
    test (WithPos s e _ (TBool t)) = Just $ Span s e :< EBool t
    test _ = Nothing

pNumber :: Parser (Expr Span)
pNumber = token test Set.empty <?> "number"
  where
    test (WithPos s e _ (TNumber t)) = Just $ Span s e :< ENumber t
    test _ = Nothing

pString :: Parser (Expr Span)
pString = token test Set.empty <?> "string"
  where
    test (WithPos s e _ (TString t)) = Just $ Span s e :< EString t
    test _ = Nothing

pArray :: Parser (Expr Span)
pArray = do
  (vs, s) <-
    spanned $
      between
        (pToken TLBracket)
        (pToken TRBracket)
        (pExpr `sepBy` pToken TComma)
  return $ s :< EArray vs

pObject :: Parser (Expr Span)
pObject = do
  (obj, s) <-
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
      return (Identifier t s, v)

pRef :: Parser (Expr Span)
pRef = do
  i <- pIdentifier
  return $ S.span i :< ERef i

pExpr' :: Parser (Expr Span)
pExpr' = do
  (e, _) <-
    spanned $
      between
        (pToken TLParenthesis)
        (pToken TRParenthesis)
        pExpr
  return e

pFunc :: Parser (Expr Span)
pFunc = do
  (_, s) <- pToken TBackslash
  args <- spanned $ many pIdentifier
  pToken TArrow
  v <- pExpr
  return $ (s S.<> S.span v) :< EFun (fst args) v

pIdentifier :: Parser (Identifier Span)
pIdentifier = token test Set.empty <?> "identifier"
  where
    test (WithPos s e _ (TIdentifier t)) = Just $ Identifier t (Span s e)
    test _ = Nothing

liftTToken :: TToken -> WithPos TToken
liftTToken = WithPos pos pos 0
  where
    pos = initialPos ""

pToken :: TToken -> Parser (TToken, Span)
pToken c = token test (Set.singleton . Tokens . nes . liftTToken $ c)
  where
    test (WithPos s e _ x) =
      if x == c
        then Just (x, Span s e)
        else Nothing
    nes x = x :| []

spanned :: Parser a -> Parser (a, Span)
spanned parser = do
  start <- getSourcePos
  x <- parser
  end <- getSourcePos
  pure (x, Span start end)

errorBundleSourcePos :: (TraversableStream a) => ParseErrorBundle a Void -> SourcePos
errorBundleSourcePos peb = do
  let pst = bundlePosState peb
  let e = NEL.head $ bundleErrors peb
  let (_, pst') = reachOffset (errorOffset e) pst
  pstateSourcePos pst'
