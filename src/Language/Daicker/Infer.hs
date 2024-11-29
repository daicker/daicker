module Language.Daicker.Infer where

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