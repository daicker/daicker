module Language.Daicker.ParserSpec (spec) where

import Language.Daicker.AST
import Language.Daicker.DLS (errorBundleSourcePos)
import Language.Daicker.Parser (pApp, pDefine, pImport, pValue)
import Language.Daicker.Span (mkSpan)
import Test.Hspec
import Text.Megaparsec

spec :: Spec
spec = do
  describe "import" $ do
    it "import a" $
      parse pImport "test" "import a" `shouldBe` Right (Import (Identifier "a" (mkSpan "test" 1 8 1 9)) (mkSpan "test" 1 1 1 9))
  describe "define" $ do
    it "define a = 1" $
      parse pDefine "test" "define a = 1"
        `shouldBe` Right
          ( Define
              (Identifier "a" (mkSpan "test" 1 8 1 9))
              ( VApp
                  Nothing
                  [VNumber 1 (mkSpan "test" 1 12 1 13)]
                  (mkSpan "test" 1 12 1 13)
              )
              (mkSpan "test" 1 1 1 13)
          )
  describe "value parser" $ do
    describe "null" $ do
      it "null" $
        parse pValue "test" "null" `shouldBe` Right (VNull (mkSpan "test" 1 1 1 5))
    describe "bool" $ do
      it "true" $
        parse pValue "test" "true" `shouldBe` Right (VBool True (mkSpan "test" 1 1 1 5))
      it "false" $
        parse pValue "test" "false" `shouldBe` Right (VBool False (mkSpan "test" 1 1 1 6))
    describe "number" $ do
      it "1" $
        parse pValue "test" "1" `shouldBe` Right (VNumber 1 (mkSpan "test" 1 1 1 2))
      it "1.5" $
        parse pValue "test" "1.5" `shouldBe` Right (VNumber 1.5 (mkSpan "test" 1 1 1 4))
    describe "string" $ do
      it "\"\"" $
        parse pValue "test" "\"\"" `shouldBe` Right (VString "" (mkSpan "test" 1 1 1 3))
      it "\"abc\"" $
        parse pValue "test" "\"abc\"" `shouldBe` Right (VString "abc" (mkSpan "test" 1 1 1 6))
    describe "array" $ do
      it "[]" $
        parse pValue "test" "[]"
          `shouldBe` Right
            ( VArray
                []
                (mkSpan "test" 1 1 1 3)
            )
      it "[1, 2]" $
        parse pValue "test" "[1, 2]"
          `shouldBe` Right
            ( VArray
                [ VNumber 1 (mkSpan "test" 1 2 1 3),
                  VNumber 2 (mkSpan "test" 1 5 1 6)
                ]
                (mkSpan "test" 1 1 1 7)
            )
    describe "object" $ do
      it "{}" $
        parse pValue "test" "{}"
          `shouldBe` Right
            ( VObject
                []
                (mkSpan "test" 1 1 1 3)
            )
      it "{\"a\": 1, \"b\": 2}" $
        parse pValue "test" "{\"a\": 1, \"b\": 2}"
          `shouldBe` Right
            ( VObject
                [ ( Identifier "a" (mkSpan "test" 1 2 1 5),
                    VNumber 1 (mkSpan "test" 1 7 1 8)
                  ),
                  ( Identifier "b" (mkSpan "test" 1 10 1 13),
                    VNumber 2 (mkSpan "test" 1 15 1 16)
                  )
                ]
                (mkSpan "test" 1 1 1 17)
            )
    describe "ref" $ do
      it "a" $
        parse pValue "test" "a" `shouldBe` Right (VRef (Identifier "a" (mkSpan "test" 1 1 1 2)) (mkSpan "test" 1 1 1 2))
      it "abc" $
        parse pValue "test" "abc" `shouldBe` Right (VRef (Identifier "abc" (mkSpan "test" 1 1 1 4)) (mkSpan "test" 1 1 1 4))
      describe "app" $ do
        it "(f 1)" $ do
          parse pValue "test" "(f 1)"
            `shouldBe` Right
              ( VApp
                  Nothing
                  [ VRef
                      (Identifier "f" (mkSpan "test" 1 2 1 3))
                      (mkSpan "test" 1 2 1 3),
                    VNumber 1 (mkSpan "test" 1 4 1 5)
                  ]
                  (mkSpan "test" 1 1 1 6)
              )
      it "(f 1 2)" $ do
        parse pValue "test" "(f 1 2)"
          `shouldBe` Right
            ( VApp
                Nothing
                [ VRef
                    (Identifier "f" (mkSpan "test" 1 2 1 3))
                    (mkSpan "test" 1 2 1 3),
                  VNumber 1 (mkSpan "test" 1 4 1 5),
                  VNumber 2 (mkSpan "test" 1 6 1 7)
                ]
                (mkSpan "test" 1 1 1 8)
            )
      it "([alpine] f 1 2)" $ do
        parse pValue "test" "([alpine] f 1 2)"
          `shouldBe` Right
            ( VApp
                (Just (Identifier "alpine" (mkSpan "test" 1 3 1 9)))
                [ VRef
                    (Identifier "f" (mkSpan "test" 1 11 1 12))
                    (mkSpan "test" 1 11 1 12),
                  VNumber 1 (mkSpan "test" 1 13 1 14),
                  VNumber 2 (mkSpan "test" 1 15 1 16)
                ]
                (mkSpan "test" 1 1 1 17)
            )
    describe "fun" $ do
      it "\\a -> a" $ do
        parse pValue "test" "\\a -> a"
          `shouldBe` Right
            ( VFun
                [Identifier "a" (mkSpan "test" 1 2 1 3)]
                ( VRef
                    (Identifier "a" (mkSpan "test" 1 7 1 8))
                    (mkSpan "test" 1 7 1 8)
                )
                (mkSpan "test" 1 1 1 8)
            )
