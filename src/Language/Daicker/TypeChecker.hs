module Language.Daicker.TypeChecker where

import Control.Comonad.Cofree (Cofree ((:<)))
import Control.Monad (join)
import Language.Daicker.AST
import Language.Daicker.Error (CodeError (CodeError))
import Language.Daicker.Executor (findDefine)
import Language.Daicker.Parser (parseModule)
import Language.Daicker.Span (Span)

validateModule :: Module Span -> Either [CodeError] ()
validateModule m@(_ :< Module name ss) = case es of
  [] -> Right ()
  _ -> Left es
  where
    es = join $ map (validateStatement m) ss

validateStatement :: Module Span -> Statement Span -> [CodeError]
validateStatement _ (_ :< SImport _) = []
validateStatement _ (_ :< SExport _) = []
validateStatement (_ :< Module _ ss) s@(_ :< SDefine (sp :< Identifier name) e t) =
  case findDefine name (filter (/= s) ss) of
    Nothing -> []
    Just _ -> [CodeError ("duplicated function name: " <> name) sp]
validateStatement _ (s :< STypeDefine name t) = []
