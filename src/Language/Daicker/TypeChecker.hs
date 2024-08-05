module Language.Daicker.TypeChecker where

import Control.Comonad.Cofree (Cofree ((:<)))
import Control.Monad (join)
import Language.Daicker.AST
import Language.Daicker.Bundler (findDefine)
import Language.Daicker.Error (StaticError (StaticError))
import Language.Daicker.Parser (parseModule)
import Language.Daicker.Span (Span)

validateModule :: Module Span -> Either [StaticError] ()
validateModule m@(_ :< Module _ _ ss) = case es of
  [] -> Right ()
  _ -> Left es
  where
    es = join $ map (validateStatement m) ss

validateStatement :: Module Span -> Statement Span -> [StaticError]
validateStatement (_ :< Module _ _ ss) s@(_ :< SDefine (_ :< Define (sp :< Identifier name) e t)) =
  case findDefine name (filter (/= s) ss) of
    Nothing -> []
    Just _ -> [StaticError ("duplicated function name: " <> name) sp]
validateStatement _ (s :< STypeDefine (_ :< TypeDefine name t)) = []
