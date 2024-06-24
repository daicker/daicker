module Language.Daicker.Parser where

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

pModule :: Parser Module
pModule = Module <$> (pToken TModule *> pIdentifier) <*> many pImport <*> many pExport <*> many pDefine <* eof

pImport :: Parser Import
pImport = do
  s <- pToken TImport
  i <- pIdentifier
  return $ Import i (S.span s S.<> S.span i)

pExport :: Parser Export
pExport = do
  s <- pToken TExport
  i <- pIdentifier
  return $ Export i (S.span s S.<> S.span i)

pDefine :: Parser Define
pDefine = do
  (_, s) <- pToken TDefine
  i <- pIdentifier
  pToken TAssign
  v <- pApp
  return $ Define i v (s S.<> S.span v)

pApp :: Parser Expr
pApp = do
  img <- optional $ between (pToken TLBracket) (pToken TRBracket) pIdentifier
  (vs, s) <- spanned $ some pExpr
  case img of
    Nothing -> return $ EApp Nothing vs s
    Just img -> return $ EApp (Just img) vs (S.span img S.<> s)

operatorTable :: [[Operator Parser Expr]]
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

binary :: TToken -> Operator Parser Expr
binary token = InfixL (f <$> pToken token)
  where
    f :: (TToken, Span) -> Expr -> Expr -> Expr
    f op a b =
      EApp
        Nothing
        [ ERef (Identifier (showTToken (fst op)) (S.span op)) (S.span op),
          a,
          b
        ]
        (S.span a S.<> S.span b)

prefix :: TToken -> Operator Parser Expr
prefix token = Prefix (f <$> pToken token)
  where
    f :: (TToken, Span) -> Expr -> Expr
    f op a =
      EApp
        Nothing
        [ ERef (Identifier (showTToken (fst op)) (S.span op)) (S.span op),
          a
        ]
        (S.span op S.<> S.span a)

pExpr :: Parser Expr
pExpr = makeExprParser pTerm operatorTable <?> "expression"

pTerm :: Parser Expr
pTerm =
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

pNull :: Parser Expr
pNull = token test Set.empty <?> "null"
  where
    test (WithPos s e _ TNull) = Just $ ENull (Span s e)
    test _ = Nothing

pBool :: Parser Expr
pBool = token test Set.empty <?> "bool"
  where
    test (WithPos s e _ (TBool t)) = Just $ EBool t (Span s e)
    test _ = Nothing

pNumber :: Parser Expr
pNumber = token test Set.empty <?> "number"
  where
    test (WithPos s e _ (TNumber t)) = Just $ ENumber t (Span s e)
    test _ = Nothing

pString :: Parser Expr
pString = token test Set.empty <?> "string"
  where
    test (WithPos s e _ (TString t)) = Just $ EString t (Span s e)
    test _ = Nothing

pArray :: Parser Expr
pArray = do
  (vs, s) <-
    spanned $
      between
        (pToken TLBracket)
        (pToken TRBracket)
        (pExpr `sepBy` pToken TComma)
  return $ EArray vs s

pObject :: Parser Expr
pObject = do
  (obj, s) <-
    spanned $
      between
        (pToken TLBrace)
        (pToken TRBrace)
        (pair `sepBy` pToken TComma)
  return $ EObject obj s
  where
    pair :: Parser (EKey, Expr)
    pair = do
      (EString t s) <- pString
      pToken TColon
      v <- pExpr
      return (Identifier t s, v)

pRef :: Parser Expr
pRef = do
  i <- pIdentifier
  return $ ERef i (S.span i)

pApp' :: Parser Expr
pApp' = do
  (EApp c vs _, s) <-
    spanned $
      between
        (pToken TLParenthesis)
        (pToken TRParenthesis)
        pApp
  return $ EApp c vs s

pFunc :: Parser Expr
pFunc = do
  (_, s) <- pToken TBackslash
  args <- spanned $ many pIdentifier
  pToken TArrow
  v <- pExpr
  return $ EFun (fst args) v (s S.<> S.span v)

pIdentifier :: Parser Identifier
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
