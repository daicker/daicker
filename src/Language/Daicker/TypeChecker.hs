module Language.Daicker.TypeChecker where

import Control.Comonad.Cofree (Cofree ((:<)))
import Control.Monad (join)
import Language.Daicker.AST
import Language.Daicker.Bundler (Bundle (Bundle))
import Language.Daicker.Error (StaticError (StaticError))
import Language.Daicker.Parser (parseModule)
import Language.Daicker.Span (Span)

validateModule :: Bundle Span -> Module Span -> Either [StaticError] ()
validateModule b m@(_ :< Module _ _ ss) = case es of
  [] -> Right ()
  _ -> Left es
  where
    es = join $ map (validateStatement b) ss

validateStatement :: Bundle Span -> NamedStatement Span -> [StaticError]
validateStatement
  (Bundle mb cm ss)
  (_ :< NamedStatement (sp :< Identifier name) s) =
    case lookup name (filter (\(name', (s', _)) -> name' == name && s' /= s) ss) of
      Nothing -> []
      Just _ -> [StaticError ("duplicated name: " <> name) sp]
