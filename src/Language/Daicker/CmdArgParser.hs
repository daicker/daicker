{-# LANGUAGE OverloadedStrings #-}

module Language.Daicker.CmdArgParser where

import Control.Comonad.Cofree (Cofree (..))
import Data.Aeson (decode)
import Data.ByteString.Lazy.Char8 (ByteString, pack)
import Data.Maybe (isJust)
import Data.Text (Text)
import Data.Void (Void)
import Language.Daicker.AST
import Language.Daicker.Error (StaticError (StaticError), fromParseErrorBundle)
import Language.Daicker.Span
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type CmdArgParser = Parsec Void Text

parseArg :: String -> Maybe ByteString -> Text -> Either [StaticError Span] (Argument Span)
parseArg fileName stdinContent src =
  case parse (pArgument stdinContent) fileName src of
    Right e -> pure e
    Left e -> Left [fromParseErrorBundle e]

pArgument :: Maybe ByteString -> CmdArgParser (Argument Span)
pArgument stdinContent =
  choice
    [ try keywordArgument,
      positionedArgument
    ]
  where
    positionedArgument :: CmdArgParser (Argument Span)
    positionedArgument = do
      (WithSpan arg s) <- withSpan $ do
        v <- pExpr stdinContent
        isExpanded <- isJust <$> optional (string "...")
        pure $ PositionedArgument isExpanded v
      pure $ s :< arg
    keywordArgument :: CmdArgParser (Argument Span)
    keywordArgument = do
      (WithSpan arg s) <- withSpan $ do
        i <- pIdentifier
        _ <- lexeme $ char '='
        v <- pExpr stdinContent
        pure $ KeywordArgument i v
      pure $ s :< arg

pIdentifier :: CmdArgParser (Identifier Span)
pIdentifier = do
  (WithSpan t s) <-
    withSpan $
      (:)
        <$> (lowerChar <|> upperChar <|> char '_')
        <*> many (alphaNumChar <|> char '_' <|> char '-')
  return $ s :< Identifier t

pExpr :: Maybe ByteString -> CmdArgParser (Expr Span)
pExpr content =
  lexeme $
    choice
      [ pNull,
        pBool,
        pNumber,
        pStdin content,
        pString,
        pImplicitString
      ]

pNull :: CmdArgParser (Expr Span)
pNull = do
  (WithSpan _ s) <- withSpan $ string "null"
  return $ s :< ENull

pBool :: CmdArgParser (Expr Span)
pBool = do
  (WithSpan b s) <-
    withSpan $
      choice
        [ False <$ string "false",
          True <$ string "true"
        ]
  return $ s :< EBool b

pNumber :: CmdArgParser (Expr Span)
pNumber = try $ do
  (WithSpan n s) <- withSpan $ L.signed sc L.scientific
  return $ s :< ENumber n

pStdin :: Maybe ByteString -> CmdArgParser (Expr Span)
pStdin content = do
  (WithSpan _ s) <- withSpan $ string "@stdin"
  content <- case content of
    Nothing -> fail "stdin is not provided"
    Just content -> pure content
  expr <- case decode content :: Maybe (Expr ()) of
    Nothing -> fail "stdin is not json format"
    Just expr -> pure expr
  return $ switchAnn (const s) expr

pString :: CmdArgParser (Expr Span)
pString = do
  (WithSpan t s) <- withSpan $ char '"' *> manyTill L.charLiteral (char '"')
  return $ s :< EString t

pImplicitString :: CmdArgParser (Expr Span)
pImplicitString = do
  (WithSpan t s) <- withSpan $ many L.charLiteral
  return $ s :< EString t

lexeme :: CmdArgParser a -> CmdArgParser a
lexeme = L.lexeme sc

sc :: CmdArgParser ()
sc = L.space space1 empty empty

withSpan :: CmdArgParser a -> CmdArgParser (WithSpan a)
withSpan lexer = do
  start@(SourcePos _ _ c1) <- getSourcePos
  x <- lexer
  end@(SourcePos _ _ c2) <- getSourcePos
  pure $ WithSpan x $ Span (fromSourcePos start) (fromSourcePos end)
