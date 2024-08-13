module Language.Daicker.TypeChecker where

import Control.Comonad.Cofree (Cofree ((:<)))
import Control.Monad (join)
import Language.Daicker.AST
import Language.Daicker.Bundler (Bundle (Bundle))
import Language.Daicker.Error (StaticError (StaticError))
import Language.Daicker.Parser (parseModule)
import Language.Daicker.Span (Span)

validateModule :: Bundle Span -> Module Span -> Either [StaticError] ()
validateModule b m@(_ :< Module _ e ss) = case es of
  [] -> Right ()
  _ -> Left es
  where
    es =
      join
        [ join $ map (validateStatement b) ss,
          case e of
            Just (_ :< Export is) -> join (map (validateExport b) is)
            Nothing -> []
        ]

validateExport :: Bundle Span -> Identifier Span -> [StaticError]
validateExport (Bundle mb cm ss) (s :< Identifier name) =
  case lookup name (filter (\(_, (_, m)) -> m == cm) ss) of
    Just _ -> []
    Nothing -> [StaticError ("not define: " <> name) s]

validateStatement :: Bundle Span -> NamedStatement Span -> [StaticError]
validateStatement
  (Bundle mb cm ss)
  (_ :< NamedStatement (sp :< Identifier name) s) =
    case lookup name (filter (\(name', (s', _)) -> name' == name && s' /= s) ss) of
      Nothing -> []
      Just _ -> [StaticError ("duplicated name: " <> name) sp]
