module Language.Daicker.TypeChecker where

import Control.Comonad.Cofree (Cofree ((:<)))
import Language.Daicker.AST (Module, Module' (..), Statement, Statement' (SDefine))
import Language.Daicker.Error (CodeError)
import Language.Daicker.Span (Span)

checkModule :: Module Span -> [CodeError]
checkModule (_ :< Module name statements) = undefined

-- where
--   doubleName = map () $ groupBy (\(Define name1 _) (Define name2 _) -> name1 == name2) defines

checkDefine :: Module Span -> Statement Span -> [CodeError]
checkDefine = undefined

-- inferDefine :: Module Span -> Define Span -> Type ()
