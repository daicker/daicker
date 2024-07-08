module Language.Daicker.ParserSpec (spec) where

import Control.Comonad.Cofree
import Language.Daicker.AST
import Language.Daicker.Lexer (mkTStream)
import Language.Daicker.Parser (Parser, pDefine, pExpr, pImport, pModule)
import Language.Daicker.Span (mkSpan)
import Test.Hspec
import Text.Megaparsec hiding (parseTest)

spec :: Spec
spec = do
  describe "import" $ do
    it "import a" $
      parseTest pImport "test" "import a" `shouldBe` Right (mkSpan "test" 1 1 1 9 :< Import (mkSpan "test" 1 8 1 9 :< Identifier "a"))
  describe "define" $ do
    it "define a = 1" $
      parseTest pDefine "test" "define a = 1"
        `shouldBe` Right
          ( mkSpan "test" 1 1 1 13
              :< Define
                (mkSpan "test" 1 8 1 9 :< Identifier "a")
                (mkSpan "test" 1 12 1 13 :< ENumber 1)
          )
    it "define f a = a" $
      parseTest pDefine "test" "define f a = a"
        `shouldBe` Right
          ( mkSpan "test" 1 1 1 15
              :< Define
                (mkSpan "test" 1 8 1 9 :< Identifier "f")
                ( mkSpan "test" 1 10 1 15
                    :< EFun
                      ( Just $
                          mkSpan "test" 1 10 1 11
                            :< PMAAnyValue
                              ( mkSpan "test" 1 10 1 11
                                  :< Identifier
                                    "a"
                              )
                      )
                      (mkSpan "test" 1 14 1 15 :< ERef (mkSpan "test" 1 14 1 15 :< Identifier "a"))
                )
          )
  describe "value parser" $ do
    describe "null" $ do
      it "null" $
        parseTest pExpr "test" "null" `shouldBe` Right (mkSpan "test" 1 1 1 5 :< ENull)
    describe "bool" $ do
      it "true" $
        parseTest pExpr "test" "true" `shouldBe` Right (mkSpan "test" 1 1 1 5 :< EBool True)
      it "false" $
        parseTest pExpr "test" "false" `shouldBe` Right (mkSpan "test" 1 1 1 6 :< EBool False)
    describe "number" $ do
      it "1" $
        parseTest pExpr "test" "1" `shouldBe` Right (mkSpan "test" 1 1 1 2 :< ENumber 1)
      it "1.5" $
        parseTest pExpr "test" "1.5" `shouldBe` Right (mkSpan "test" 1 1 1 4 :< ENumber 1.5)
    describe "string" $ do
      it "\"\"" $
        parseTest pExpr "test" "\"\"" `shouldBe` Right (mkSpan "test" 1 1 1 3 :< EString "")
      it "\"abc\"" $
        parseTest pExpr "test" "\"abc\"" `shouldBe` Right (mkSpan "test" 1 1 1 6 :< EString "abc")
    describe "array" $ do
      it "[]" $
        parseTest pExpr "test" "[]"
          `shouldBe` Right
            ( mkSpan "test" 1 1 1 3 :< EArray []
            )
      it "[1, 2]" $
        parseTest pExpr "test" "[1, 2]"
          `shouldBe` Right
            ( mkSpan "test" 1 1 1 7
                :< EArray
                  [ mkSpan "test" 1 2 1 3 :< ENumber 1,
                    mkSpan "test" 1 5 1 6 :< ENumber 2
                  ]
            )
    describe "object" $ do
      it "{}" $
        parseTest pExpr "test" "{}"
          `shouldBe` Right
            ( mkSpan "test" 1 1 1 3 :< EObject []
            )
      it "{\"a\": 1, \"b\": 2}" $
        parseTest pExpr "test" "{\"a\": 1, \"b\": 2}"
          `shouldBe` Right
            ( mkSpan "test" 1 1 1 17
                :< EObject
                  [ ( mkSpan "test" 1 2 1 5 :< Identifier "a",
                      mkSpan "test" 1 7 1 8 :< ENumber 1
                    ),
                    ( mkSpan "test" 1 10 1 13 :< Identifier "b",
                      mkSpan "test" 1 15 1 16 :< ENumber 2
                    )
                  ]
            )
    describe "ref" $ do
      it "a" $
        parseTest pExpr "test" "a" `shouldBe` Right (mkSpan "test" 1 1 1 2 :< ERef (mkSpan "test" 1 1 1 2 :< Identifier "a"))
      it "abc" $
        parseTest pExpr "test" "abc" `shouldBe` Right (mkSpan "test" 1 1 1 4 :< ERef (mkSpan "test" 1 1 1 4 :< Identifier "abc"))
      describe "app" $ do
        it "(f 1)" $ do
          parseTest pExpr "test" "(f 1)"
            `shouldBe` Right
              ( mkSpan "test" 1 1 1 6
                  :< EApp
                    Nothing
                    ( mkSpan "test" 1 2 1 3
                        :< ERef
                          (mkSpan "test" 1 2 1 3 :< Identifier "f")
                    )
                    (mkSpan "test" 1 4 1 5 :< ENumber 1)
              )
      it "1 + 2" $ do
        parseTest pExpr "test" "1 + 2"
          `shouldBe` Right
            ( mkSpan "test" 1 1 1 6
                :< EApp
                  Nothing
                  ( mkSpan "test" 1 3 1 4
                      :< ERef
                        (mkSpan "test" 1 3 1 4 :< Identifier "+")
                  )
                  ( mkSpan "test" 1 1 1 6
                      :< EArray
                        [ mkSpan "test" 1 1 1 2 :< ENumber 1,
                          mkSpan "test" 1 5 1 6 :< ENumber 2
                        ]
                  )
            )
      it "(#alpine f 1 2)" $ do
        parseTest pExpr "test" "(#alpine f 1 2)"
          `shouldBe` Right
            ( mkSpan "test" 1 1 1 16
                :< EApp
                  (Just (mkSpan "test" 1 2 1 9 :< Identifier "alpine"))
                  ( mkSpan "test" 1 10 1 11
                      :< ERef
                        (mkSpan "test" 1 10 1 11 :< Identifier "f")
                  )
                  ( mkSpan "test" 1 12 1 15
                      :< EArray
                        [ mkSpan "test" 1 12 1 13 :< ENumber 1,
                          mkSpan "test" 1 14 1 15 :< ENumber 2
                        ]
                  )
            )
    describe "fun" $ do
      it "\\a -> a" $ do
        parseTest pExpr "test" "\\a -> a"
          `shouldBe` Right
            ( mkSpan "test" 1 1 1 8
                :< EFun
                  (Just $ mkSpan "test" 1 2 1 3 :< PMAAnyValue (mkSpan "test" 1 2 1 3 :< Identifier "a"))
                  ( mkSpan "test" 1 7 1 8
                      :< ERef
                        (mkSpan "test" 1 7 1 8 :< Identifier "a")
                  )
            )

parseTest :: Parser a -> String -> String -> Either String a
parseTest parser fileName src = do
  case mkTStream fileName src of
    Left e -> Left $ errorBundlePretty e
    Right ts ->
      case parse parser fileName ts of
        Right m -> Right m
        Left e -> Left $ errorBundlePretty e
