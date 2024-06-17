module Language.Daicker.Executor where

import Data.Foldable (find)
import Language.Daicker.AST
import System.Process (callCommand)

class Evaluatable a where
  eval :: a -> Value

findDefine :: String -> Module -> Maybe Define
findDefine name (Module _ _ _ ds) = find (\(Define (Identifier n _) _ _) -> name == n) ds

execDefine :: Define -> IO ()
execDefine (Define _ (VApp _ [_, VString s _] _) _) = callCommand s

stdLib :: [(String, Value -> IO ())]
stdLib = [("$", \(VString s _) -> callCommand s)]
