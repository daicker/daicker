module Language.Daicker.Validator where

import Control.Comonad.Cofree (Cofree ((:<)))
import Control.Monad (join)
import Control.Monad.Except (ExceptT, liftEither)
import Data.Text (Text)
import Language.Daicker.AST
import Language.Daicker.Bundler (Environment (currentModule), findExpr, loadEnvironment)
import Language.Daicker.Error (StaticError (StaticError))
import Language.Daicker.Parser (pModule, parse)
import Language.Daicker.Span (Span, spanPretty)

validate :: String -> Text -> ExceptT [StaticError Span] IO ()
validate fileName src = do
  (m@(_ :< Module is _ _), _) <- liftEither $ parse pModule fileName src
  env <- loadEnvironment m
  liftEither $ validateModule env m

validateModule :: (Eq a) => Environment a -> Module a -> Either [StaticError a] ()
validateModule b m@(_ :< Module _ e ss) = case es of
  [] -> Right ()
  _ -> Left es
  where
    es =
      join
        [ case e of
            Just (_ :< Export is) -> join (map (validateExport b) is)
            Nothing -> []
        ]

validateExport :: (Eq a) => Environment a -> Identifier a -> [StaticError a]
validateExport bundle (s :< Identifier name) =
  case findExpr bundle name of
    Just _ -> []
    Nothing -> [StaticError ("not define: " <> name) s]
