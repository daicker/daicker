module Language.Daicker.ParserSpec (spec) where

import Test.Hspec
import Text.Megaparsec
import Language.Daicker.Parser (pValue, pImport, pDefine)
import Language.Daicker.Span (mkSpan)
import Language.Daicker.AST
import Language.Daicker.DLS (errorBundleSourcePos)

spec :: Spec
spec = do
  describe "import" $ do
    it "import a"
      $ parse pImport "test" "import a" `shouldBe` Right (Import (Identifier "a" (mkSpan "test" 1 8 1 9)) (mkSpan "test" 1 1 1 9))
  describe "define" $ do
    it "a = 1"
      $ parse pDefine "test" "a = 1" `shouldBe`
        Right (Define
          (Identifier "a" (mkSpan "test" 1 1 1 2))
          (VNumber 1 (mkSpan "test" 1 5 1 6))
          (mkSpan "test" 1 1 1 6))
  describe "value parser" $ do
    describe "null" $ do
      it "null"
        $ parse pValue "test" "null" `shouldBe` Right (VNull (mkSpan "test" 1 1 1 5))
    describe "bool" $ do
      it "true"
        $ parse pValue "test" "true" `shouldBe` Right (VBool True (mkSpan "test" 1 1 1 5))
      it "false"
        $ parse pValue "test" "false" `shouldBe` Right (VBool False (mkSpan "test" 1 1 1 6))
    describe "number" $ do
      it "1"
        $ parse pValue "test" "1" `shouldBe` Right (VNumber 1 (mkSpan "test" 1 1 1 2))
      it "1.5"
        $ parse pValue "test" "1.5" `shouldBe` Right (VNumber 1.5 (mkSpan "test" 1 1 1 4))
    describe "string" $ do
      it "\"\""
        $ parse pValue "test" "\"\"" `shouldBe` Right (VString "" (mkSpan "test" 1 1 1 3))
      it "\"abc\""
        $ parse pValue "test" "\"abc\"" `shouldBe` Right (VString "abc" (mkSpan "test" 1 1 1 6))
    describe "ref" $ do
      it "a"
        $ parse pValue "test" "a" `shouldBe` Right (VRef (Identifier "a" (mkSpan "test" 1 1 1 2)) (mkSpan "test" 1 1 1 2))
      it "abc"
        $ parse pValue "test" "abc" `shouldBe` Right (VRef (Identifier "abc" (mkSpan "test" 1 1 1 4)) (mkSpan "test" 1 1 1 4))
