module Language.Daicker.TypeChecker where

import Control.Comonad.Cofree (Cofree ((:<)))
import Control.Monad (join)
import Language.Daicker.AST
import Language.Daicker.Bundler (Bundle (Bundle), findDefine)
import Language.Daicker.Error (StaticError (StaticError))
import Language.Daicker.Parser (parseModule)
import Language.Daicker.Span (Span)

validateModule :: Bundle Span -> Module Span -> Either [StaticError] ()
validateModule b m@(_ :< Module _ _ ss) = case es of
  [] -> Right ()
  _ -> Left es
  where
    es = join $ map (validateStatement b) ss

validateStatement :: Bundle Span -> Statement Span -> [StaticError]
validateStatement (Bundle ms es cm) s@(_ :< SDefine (_ :< Define (sp :< Identifier name) e t)) =
  case lookup name (filter (\(name', (e', _)) -> name' == name && e' /= e) es) of
    Nothing -> []
    Just _ -> [StaticError ("duplicated function name: " <> name) sp]
validateStatement _ (s :< STypeDefine (_ :< TypeDefine name t)) = []
