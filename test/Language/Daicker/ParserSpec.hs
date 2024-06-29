module Language.Daicker.ParserSpec (spec) where

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
      parseTest pImport "test" "import a" `shouldBe` Right (Import (Identifier "a" (mkSpan "test" 1 8 1 9)) (mkSpan "test" 1 1 1 9))
  describe "define" $ do
    it "define a = 1" $
      parseTest pDefine "test" "define a = 1"
        `shouldBe` Right
          ( Define
              (Identifier "a" (mkSpan "test" 1 8 1 9))
              (ENumber 1 (mkSpan "test" 1 12 1 13))
              (mkSpan "test" 1 1 1 13)
          )
    it "define f a = a" $
      parseTest pDefine "test" "define f a = a"
        `shouldBe` Right
          ( Define
              (Identifier "f" (mkSpan "test" 1 8 1 9))
              ( EFun
                  [ Identifier
                      "a"
                      (mkSpan "test" 1 10 1 11)
                  ]
                  (ERef (Identifier "a" (mkSpan "test" 1 14 1 15)) (mkSpan "test" 1 14 1 15))
                  (mkSpan "test" 1 10 1 15)
              )
              (mkSpan "test" 1 1 1 15)
          )
  describe "value parser" $ do
    describe "null" $ do
      it "null" $
        parseTest pExpr "test" "null" `shouldBe` Right (ENull (mkSpan "test" 1 1 1 5))
    describe "bool" $ do
      it "true" $
        parseTest pExpr "test" "true" `shouldBe` Right (EBool True (mkSpan "test" 1 1 1 5))
      it "false" $
        parseTest pExpr "test" "false" `shouldBe` Right (EBool False (mkSpan "test" 1 1 1 6))
    describe "number" $ do
      it "1" $
        parseTest pExpr "test" "1" `shouldBe` Right (ENumber 1 (mkSpan "test" 1 1 1 2))
      it "1.5" $
        parseTest pExpr "test" "1.5" `shouldBe` Right (ENumber 1.5 (mkSpan "test" 1 1 1 4))
    describe "string" $ do
      it "\"\"" $
        parseTest pExpr "test" "\"\"" `shouldBe` Right (EString "" (mkSpan "test" 1 1 1 3))
      it "\"abc\"" $
        parseTest pExpr "test" "\"abc\"" `shouldBe` Right (EString "abc" (mkSpan "test" 1 1 1 6))
    describe "array" $ do
      it "[]" $
        parseTest pExpr "test" "[]"
          `shouldBe` Right
            ( EArray
                []
                (mkSpan "test" 1 1 1 3)
            )
      it "[1, 2]" $
        parseTest pExpr "test" "[1, 2]"
          `shouldBe` Right
            ( EArray
                [ ENumber 1 (mkSpan "test" 1 2 1 3),
                  ENumber 2 (mkSpan "test" 1 5 1 6)
                ]
                (mkSpan "test" 1 1 1 7)
            )
    describe "object" $ do
      it "{}" $
        parseTest pExpr "test" "{}"
          `shouldBe` Right
            ( EObject
                []
                (mkSpan "test" 1 1 1 3)
            )
      it "{\"a\": 1, \"b\": 2}" $
        parseTest pExpr "test" "{\"a\": 1, \"b\": 2}"
          `shouldBe` Right
            ( EObject
                [ ( Identifier "a" (mkSpan "test" 1 2 1 5),
                    ENumber 1 (mkSpan "test" 1 7 1 8)
                  ),
                  ( Identifier "b" (mkSpan "test" 1 10 1 13),
                    ENumber 2 (mkSpan "test" 1 15 1 16)
                  )
                ]
                (mkSpan "test" 1 1 1 17)
            )
    describe "ref" $ do
      it "a" $
        parseTest pExpr "test" "a" `shouldBe` Right (ERef (Identifier "a" (mkSpan "test" 1 1 1 2)) (mkSpan "test" 1 1 1 2))
      it "abc" $
        parseTest pExpr "test" "abc" `shouldBe` Right (ERef (Identifier "abc" (mkSpan "test" 1 1 1 4)) (mkSpan "test" 1 1 1 4))
      describe "app" $ do
        it "(f 1)" $ do
          parseTest pExpr "test" "(f 1)"
            `shouldBe` Right
              ( EApp
                  Nothing
                  [ ERef
                      (Identifier "f" (mkSpan "test" 1 2 1 3))
                      (mkSpan "test" 1 2 1 3),
                    ENumber 1 (mkSpan "test" 1 4 1 5)
                  ]
                  (mkSpan "test" 1 2 1 5)
              )
      it "1 + 2" $ do
        parseTest pExpr "test" "1 + 2"
          `shouldBe` Right
            ( EApp
                Nothing
                [ ERef
                    (Identifier "+" (mkSpan "test" 1 3 1 4))
                    (mkSpan "test" 1 3 1 4),
                  ENumber 1 (mkSpan "test" 1 1 1 2),
                  ENumber 2 (mkSpan "test" 1 5 1 6)
                ]
                (mkSpan "test" 1 1 1 6)
            )
      it "(#alpine f 1 2)" $ do
        parseTest pExpr "test" "(#alpine f 1 2)"
          `shouldBe` Right
            ( EApp
                (Just (Identifier "alpine" (mkSpan "test" 1 2 1 9)))
                [ ERef
                    (Identifier "f" (mkSpan "test" 1 10 1 11))
                    (mkSpan "test" 1 10 1 11),
                  ENumber 1 (mkSpan "test" 1 12 1 13),
                  ENumber 2 (mkSpan "test" 1 14 1 15)
                ]
                (mkSpan "test" 1 2 1 15)
            )
    describe "fun" $ do
      it "\\a -> a" $ do
        parseTest pExpr "test" "\\a -> a"
          `shouldBe` Right
            ( EFun
                [Identifier "a" (mkSpan "test" 1 2 1 3)]
                ( ERef
                    (Identifier "a" (mkSpan "test" 1 7 1 8))
                    (mkSpan "test" 1 7 1 8)
                )
                (mkSpan "test" 1 1 1 8)
            )

parseTest :: Parser a -> String -> String -> Either String a
parseTest parser fileName src = do
  case mkTStream fileName src of
    Left e -> Left $ errorBundlePretty e
    Right ts ->
      case parse parser fileName ts of
        Right m -> Right m
        Left e -> Left $ errorBundlePretty e
