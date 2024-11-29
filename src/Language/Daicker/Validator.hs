module Language.Daicker.Validator where

import Control.Comonad.Cofree (Cofree ((:<)))
import Control.Monad (join)
import Language.Daicker.AST
import Language.Daicker.Bundler (Bundle (Bundle), ModuleBundle (currentModule), findExpr)
import Language.Daicker.Error (StaticError (StaticError))
import Language.Daicker.Parser (parse)
import Language.Daicker.Span (Span, spanPretty)

validateModule :: (Eq a) => Bundle a -> Module a -> Either [StaticError a] ()
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

validateExport :: (Eq a) => Bundle a -> Identifier a -> [StaticError a]
validateExport bundle (s :< Identifier name) =
  case findExpr bundle name of
    Just _ -> []
    Nothing -> [StaticError ("not define: " <> name) s]
