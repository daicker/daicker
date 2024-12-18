{-# LANGUAGE OverloadedStrings #-}

module Command.Daicker.Parser where

import Control.Comonad.Cofree (Cofree (..))
import Data.Aeson (decode)
import Data.ByteString.Lazy.Char8 (ByteString, pack)
import Data.Maybe (isJust)
import Data.Text (Text)
import Data.Void (Void)
import Language.Daicker.AST
import Language.Daicker.Error (StaticError (StaticError), fromParseErrorBundle)
import Language.Daicker.Parser (Parser, TokenKind (..), lexeme, pBool, pImage, pNull, pNumber, pString, parse, spanned, tLowerIdentifier, token, tupleToCofree)
import Language.Daicker.Span
import Text.Megaparsec
  ( MonadParsec (try),
    choice,
    many,
    optional,
  )
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

parseArg :: String -> Maybe ByteString -> Text -> Either [StaticError Span] (Argument Span)
parseArg fileName stdinContent src =
  case parse (pArgument stdinContent) fileName src of
    Right (e, _) -> pure e
    Left e -> Left e

pArgument :: Maybe ByteString -> Parser (Argument Span)
pArgument stdinContent =
  choice
    [ try keywordArgument,
      positionedArgument
    ]
  where
    positionedArgument :: Parser (Argument Span)
    positionedArgument = do
      (s, arg) <- spanned $ do
        v <- pExpr stdinContent
        isExpanded <- isJust <$> optional (lexeme $ token TKOp $ string "...")
        pure $ PositionedArgument isExpanded v
      pure $ s :< arg
    keywordArgument :: Parser (Argument Span)
    keywordArgument = do
      key@(s1 :< _) <- tupleToCofree Identifier <$> lexeme (spanned (tLowerIdentifier TKParameter))
      _ <- lexeme $ token TKSep $ char '='
      (s2, value) <- spanned $ pExpr stdinContent
      pure $ s1 `union` s2 :< KeywordArgument key value

pExpr :: Maybe ByteString -> Parser (Expr Span)
pExpr content =
  lexeme $
    choice
      [ pNull,
        pBool,
        pNumber,
        pImage,
        pStdin content,
        pString,
        pImplicitString
      ]

pStdin :: Maybe ByteString -> Parser (Expr Span)
pStdin content = do
  (s, _) <- spanned $ string "@stdin"
  content <- case content of
    Nothing -> fail "stdin is not provided"
    Just content -> pure content
  expr <- case decode content :: Maybe (Expr ()) of
    Nothing -> fail "stdin is not json format"
    Just expr -> pure expr
  return $ switchAnn (const s) expr

pImplicitString :: Parser (Expr Span)
pImplicitString = do
  (s, t) <- spanned $ many L.charLiteral
  return $ s :< EString t
