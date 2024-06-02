module Language.Daicker.ParserSpec (spec) where

import Test.Hspec
import Text.Megaparsec
import Language.Daicker.Parser (pValue)
import Language.Daicker.Span (mkSpan)
import Language.Daicker.AST
import Language.Daicker.AST (Value(VNumber))

spec :: Spec
spec = do
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
