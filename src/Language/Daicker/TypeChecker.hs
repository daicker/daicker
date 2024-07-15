module Language.Daicker.TypeChecker where

import Control.Comonad.Cofree (Cofree ((:<)))
import Language.Daicker.AST (Define, Define' (Define), Module, Module' (..))
import Language.Daicker.Error (CodeError)
import Language.Daicker.Span (Span)

checkModule :: Module Span -> [CodeError]
checkModule (_ :< Module name imports exports defines) = undefined

-- where
--   doubleName = map () $ groupBy (\(Define name1 _) (Define name2 _) -> name1 == name2) defines

checkDefine :: Module Span -> Define Span -> [CodeError]
checkDefine = undefined

-- inferDefine :: Module Span -> Define Span -> Type ()
