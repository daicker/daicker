module Language.Daicker.TypeChecker where

import Control.Comonad.Cofree (Cofree ((:<)))
import Control.Monad (join)
import Language.Daicker.AST
import Language.Daicker.Bundler (Bundle (Bundle))
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
        [ join $ map (validateStatement b) ss,
          case e of
            Just (_ :< Export is) -> join (map (validateExport b) is)
            Nothing -> []
        ]

validateExport :: (Eq a) => Bundle a -> Identifier a -> [StaticError a]
validateExport (Bundle _ _ cm ss _) (s :< Identifier name) =
  case lookup name (filter (\(_, (_, m)) -> m == cm) ss) of
    Just _ -> []
    Nothing -> [StaticError ("not define: " <> name) s]

validateStatement :: (Eq a) => Bundle a -> Statement a -> [StaticError a]
validateStatement
  (Bundle _ _ cm ss _)
  s@(_ :< SExpr (sp :< Identifier name) _) =
    case lookup
      name
      ( filter
          ( \(name', (s', _)) ->
              name' == name
                && s' /= s
                && s' ~= s
          )
          ss
      ) of
      Nothing -> []
      Just _ -> [StaticError ("duplicated name: " <> name) sp]

-- infer :: Bundle Span -> Expr Span -> Either StaticError (Type Span)
-- infer b e = case e of
--   (s :< ENull) -> pure $ s :< TNull
--   (s :< EBool _) -> pure $ s :< TBool
--   (s :< ENumber _) -> pure $ s :< TNumber
--   (s :< EString _) -> pure $ s :< TString
--   (s :< EArray es) -> do
--     ts <- mapM (infer b) es
--     pure $ s :< (s :< TTuple ts) `TOr` (s :< TArray (arrayElementType s ts))
--   (s :< EObject ps) -> do
--     ts <- mapM (infer b . snd) ps
--     pure $ s :< TObject (zip (map fst ps) ts)
--   _ -> error "Type inference not implemented for this expression"

-- arrayElementType :: a -> [Type a] -> Type a
-- arrayElementType m ts = case ts of
--   [] -> m :< TAny
--   [t] -> t
--   (t@(_ :< t') : ts) ->
--     if t `typeEqual` arrayElementType m ts
--       then m :< t'
--       else m :< TAny

-- typeEqual :: Type a -> Type a -> Bool
-- typeEqual (_ :< t1) (_ :< t2) = case (t1, t2) of
--   (TVar (_ :< Identifier i1), TVar (_ :< Identifier i2)) -> i1 == i2
--   (TObject obj1, TObject obj2) -> all (uncurry typeEqual) (zip obj1 obj2)
--   (TParameterized a [a], TBool) -> True
--   (TNumber, TNumber) -> True
--   (TString, TString) -> True
--   (TTuple ts1, TTuple ts2) -> all (uncurry typeEqual) (zip ts1 ts2)
--   (TArray t1, TArray t2) -> t1 `typeEqual` t2
--   (TObject ps1, TObject ps2) -> all (\((_ :< k1, v1), (_ :< k2, v2)) -> k1 == k2 && v1 `typeEqual` v2) (zip ps1 ps2)
--   (TMap v1, TMap v2) -> v1 `typeEqual` v2
--   (TFun ts1 t1 e1, TFun ts2 t2 e2) -> all (uncurry typeEqual) (zip ts1 ts2) && t1 `typeEqual` t2 && e1 == e2
--   (TRef (_ :< name1), TRef (_ :< name2)) -> name1 == name2
--   (TOr t1 t2, TOr t3 t4) -> t1 `typeEqual` t3 && t2 `typeEqual` t4
--   (TAny, TAny) -> True
--   _ -> False
