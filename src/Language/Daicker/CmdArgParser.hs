module Language.Daicker.CmdArgParser where

import Control.Comonad.Cofree (Cofree (..))
import Data.Aeson (decode)
import Data.ByteString.Lazy.Char8 (pack)
import Data.Void (Void)
import Language.Daicker.AST
import Language.Daicker.Error (CodeError, fromParseErrorBundle)
import Language.Daicker.Span
import Text.Megaparsec
import Text.Megaparsec.Char (char, space1, string)
import qualified Text.Megaparsec.Char.Lexer as L

type CmdArgParser = Parsec Void String

parseArg :: String -> Maybe String -> String -> Either [CodeError] (Expr Span)
parseArg fileName stdinContent src =
  case parse (pArg stdinContent) fileName src of
    Right e -> pure e
    Left e -> Left [fromParseErrorBundle e]

pArg :: Maybe String -> CmdArgParser (Expr Span)
pArg content =
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

pStdin :: Maybe String -> CmdArgParser (Expr Span)
pStdin content = do
  (WithSpan _ s) <- withSpan $ string "@stdin"
  content <- case content of
    Nothing -> fail "stdin is not provided"
    Just content -> pure content
  expr <- case decode (pack content) :: Maybe (Expr ()) of
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
