{-# LANGUAGE OverloadedStrings #-}

module Language.Daicker.ParserSpec (spec) where

import Control.Comonad.Cofree
import Data.Text (Text)
import Language.Daicker.AST
import Language.Daicker.Error (codeErrorPretty, staticErrorListPretty)
import Language.Daicker.Parser
import Language.Daicker.Span (mkSpan)
import Test.Hspec

spec :: Spec
spec = do
  describe "expression" $ do
    it "null" $ do
      parse pExpr "test" "null"
        `shouldBe` Right
          ( mkSpan "test" 1 1 1 5
              :< ENull,
            [Token TKNull (mkSpan "test" 1 1 1 5)]
          )
    it "true" $ do
      parse pExpr "test" "true"
        `shouldBe` Right
          ( mkSpan "test" 1 1 1 5
              :< EBool True,
            [Token TKBool (mkSpan "test" 1 1 1 5)]
          )
    it "false" $ do
      parse pExpr "test" "false"
        `shouldBe` Right
          ( mkSpan "test" 1 1 1 6
              :< EBool False,
            [Token TKBool (mkSpan "test" 1 1 1 6)]
          )
    it "positive number" $ do
      parse pExpr "test" "1"
        `shouldBe` Right
          ( mkSpan "test" 1 1 1 2
              :< ENumber 1,
            [Token TKNumber (mkSpan "test" 1 1 1 2)]
          )
    it "negative number" $ do
      parse pExpr "test" "-1"
        `shouldBe` Right
          ( mkSpan "test" 1 1 1 3
              :< ENumber (-1),
            [Token TKNumber (mkSpan "test" 1 1 1 3)]
          )
    it "positive float" $ do
      parse pExpr "test" "1.5"
        `shouldBe` Right
          ( mkSpan "test" 1 1 1 4
              :< ENumber 1.5,
            [Token TKNumber (mkSpan "test" 1 1 1 4)]
          )
    it "negative float" $ do
      parse pExpr "test" "-1.5"
        `shouldBe` Right
          ( mkSpan "test" 1 1 1 5
              :< ENumber (-1.5),
            [Token TKNumber (mkSpan "test" 1 1 1 5)]
          )
    it "empty string" $ do
      parse pExpr "test" "\"\""
        `shouldBe` Right
          ( mkSpan "test" 1 1 1 3
              :< EString "",
            [Token TKString (mkSpan "test" 1 1 1 3)]
          )
    it "string" $ do
      parse pExpr "test" "\"abc\""
        `shouldBe` Right
          ( mkSpan "test" 1 1 1 6
              :< EString "abc",
            [Token TKString (mkSpan "test" 1 1 1 6)]
          )
    it "empty array" $ do
      parse pExpr "test" "[]"
        `shouldBe` Right
          ( mkSpan "test" 1 1 1 3
              :< EArray [],
            [ Token TKSep (mkSpan "test" 1 1 1 2),
              Token TKSep (mkSpan "test" 1 2 1 3)
            ]
          )
    it "array" $ do
      parse pExpr "test" "[1, 2]"
        `shouldBe` Right
          ( mkSpan "test" 1 1 1 7
              :< EArray
                [ mkSpan "test" 1 2 1 3 :< ENumber 1,
                  mkSpan "test" 1 5 1 6 :< ENumber 2
                ],
            [ Token TKSep (mkSpan "test" 1 1 1 2),
              Token TKNumber (mkSpan "test" 1 2 1 3),
              Token TKSep (mkSpan "test" 1 3 1 4),
              Token TKNumber (mkSpan "test" 1 5 1 6),
              Token TKSep (mkSpan "test" 1 6 1 7)
            ]
          )
    it "empty object" $ do
      parse pExpr "test" "{}"
        `shouldBe` Right
          ( mkSpan "test" 1 1 1 3
              :< EObject [],
            [ Token TKSep (mkSpan "test" 1 1 1 2),
              Token TKSep (mkSpan "test" 1 2 1 3)
            ]
          )
    it "object" $ do
      parse pExpr "test" "{\"a\": 1, \"b\": 2}"
        `shouldBe` Right
          ( mkSpan "test" 1 1 1 17
              :< EObject
                [ ( mkSpan "test" 1 2 1 5 :< EString "a",
                    mkSpan "test" 1 7 1 8 :< ENumber 1
                  ),
                  ( mkSpan "test" 1 10 1 13 :< EString "b",
                    mkSpan "test" 1 15 1 16 :< ENumber 2
                  )
                ],
            [ Token TKSep (mkSpan "test" 1 1 1 2),
              Token TKString (mkSpan "test" 1 2 1 5),
              Token TKSep (mkSpan "test" 1 5 1 6),
              Token TKNumber (mkSpan "test" 1 7 1 8),
              Token TKSep (mkSpan "test" 1 8 1 9),
              Token TKString (mkSpan "test" 1 10 1 13),
              Token TKSep (mkSpan "test" 1 13 1 14),
              Token TKNumber (mkSpan "test" 1 15 1 16),
              Token TKSep (mkSpan "test" 1 16 1 17)
            ]
          )
    it "single alphabet var" $ do
      parse pExpr "test" "a"
        `shouldBe` Right
          ( mkSpan "test" 1 1 1 2
              :< EVar (mkSpan "test" 1 1 1 2 :< Identifier "a"),
            [Token TKVar (mkSpan "test" 1 1 1 2)]
          )
    it "multiple alphabet var" $ do
      parse pExpr "test" "abc"
        `shouldBe` Right
          ( mkSpan "test" 1 1 1 4
              :< EVar (mkSpan "test" 1 1 1 4 :< Identifier "abc"),
            [Token TKVar (mkSpan "test" 1 1 1 4)]
          )
    it "many word var" $ do
      parse pExpr "test" "abc-def"
        `shouldBe` Right
          ( mkSpan "test" 1 1 1 8
              :< EVar (mkSpan "test" 1 1 1 8 :< Identifier "abc-def"),
            [Token TKVar (mkSpan "test" 1 1 1 8)]
          )
    it "lambda" $ do
      parse pExpr "test" "\\(a) -> a"
        `shouldBe` Right
          ( mkSpan "test" 1 1 1 10
              :< ELambda
                [ mkSpan "test" 1 3 1 4
                    :< PositionedParameter
                      (mkSpan "test" 1 3 1 4 :< Identifier "a")
                      False
                      False
                      Nothing
                      Nothing
                ]
                (mkSpan "test" 1 9 1 10 :< EVar (mkSpan "test" 1 9 1 10 :< Identifier "a")),
            [ Token TKSep (mkSpan "test" 1 1 1 2),
              Token TKSep (mkSpan "test" 1 2 1 3),
              Token TKParameter (mkSpan "test" 1 3 1 4),
              Token TKSep (mkSpan "test" 1 4 1 5),
              Token TKSep (mkSpan "test" 1 6 1 8),
              Token TKVar (mkSpan "test" 1 9 1 10)
            ]
          )
    it "lambda with default value" $ do
      parse pExpr "test" "\\(a = 1) -> a"
        `shouldBe` Right
          ( mkSpan "test" 1 1 1 14
              :< ELambda
                [ mkSpan "test" 1 3 1 8
                    :< PositionedParameter
                      (mkSpan "test" 1 3 1 4 :< Identifier "a")
                      False
                      False
                      Nothing
                      (Just (mkSpan "test" 1 7 1 8 :< ENumber 1))
                ]
                (mkSpan "test" 1 13 1 14 :< EVar (mkSpan "test" 1 13 1 14 :< Identifier "a")),
            [ Token TKSep (mkSpan "test" 1 1 1 2),
              Token TKSep (mkSpan "test" 1 2 1 3),
              Token TKParameter (mkSpan "test" 1 3 1 4),
              Token TKOp (mkSpan "test" 1 5 1 6),
              Token TKNumber (mkSpan "test" 1 7 1 8),
              Token TKSep (mkSpan "test" 1 8 1 9),
              Token TKSep (mkSpan "test" 1 10 1 12),
              Token TKVar (mkSpan "test" 1 13 1 14)
            ]
          )

-- describe "import" $ do
--   it "import * from \"test.daic\"" $
--     parseTest pImport "test" "import * from \"test.daic\""
--       `shouldBe` Right
--         ( mkSpan "test" 1 1 1 26
--             :< WildImport
--               (mkSpan "test" 1 15 1 26 :< LocalFile "test.daic")
--         )
-- describe "define" $ do
--   it "func a = 1" $
--     parseTest pExprOrExprTypeStatement "test" "func a = 1"
--       `shouldBe` Right
--         ( mkSpan "test" 1 1 1 11
--             :< NamedStatement
--               (mkSpan "test" 1 6 1 7 :< Identifier "a")
--               ( mkSpan "test" 1 1 1 11
--                   :< SExpr
--                     (mkSpan "test" 1 10 1 11 :< ENumber 1)
--               )
--         )
--   it "func f a = a" $
--     parseTest pExprOrExprTypeStatement "test" "func f a = a"
--       `shouldBe` Right
--         ( mkSpan "test" 1 1 1 13
--             :< NamedStatement
--               (mkSpan "test" 1 6 1 7 :< Identifier "f")
--               ( mkSpan "test" 1 1 1 13
--                   :< SExpr
--                     ( mkSpan "test" 1 8 1 13
--                         :< EFun
--                           [ mkSpan "test" 1 8 1 9
--                               :< PMAAnyValue
--                                 ( mkSpan "test" 1 8 1 9
--                                     :< Identifier
--                                       "a"
--                                 )
--                           ]
--                           (mkSpan "test" 1 12 1 13 :< ERef (mkSpan "test" 1 12 1 13 :< Identifier "a"))
--                           False
--                     )
--               )
--         )
-- describe "value parser" $ do
--   describe "null" $ do
--     it "null" $
--       parseTest pExpr "test" "null" `shouldBe` Right (mkSpan "test" 1 1 1 5 :< ENull)
--   describe "bool" $ do
--     it "true" $
--       parseTest pExpr "test" "true" `shouldBe` Right (mkSpan "test" 1 1 1 5 :< EBool True)
--     it "false" $
--       parseTest pExpr "test" "false" `shouldBe` Right (mkSpan "test" 1 1 1 6 :< EBool False)
--   describe "number" $ do
--     it "1" $
--       parseTest pExpr "test" "1" `shouldBe` Right (mkSpan "test" 1 1 1 2 :< ENumber 1)
--     it "1.5" $
--       parseTest pExpr "test" "1.5" `shouldBe` Right (mkSpan "test" 1 1 1 4 :< ENumber 1.5)
--   describe "string" $ do
--     it "\"\"" $
--       parseTest pExpr "test" "\"\"" `shouldBe` Right (mkSpan "test" 1 1 1 3 :< EString "")
--     it "\"abc\"" $
--       parseTest pExpr "test" "\"abc\"" `shouldBe` Right (mkSpan "test" 1 1 1 6 :< EString "abc")
--   describe "array" $ do
--     it "[]" $
--       parseTest pExpr "test" "[]"
--         `shouldBe` Right
--           ( mkSpan "test" 1 1 1 3 :< EArray []
--           )
--     it "[1, 2]" $
--       parseTest pExpr "test" "[1, 2]"
--         `shouldBe` Right
--           ( mkSpan "test" 1 1 1 7
--               :< EArray
--                 [ mkSpan "test" 1 2 1 3 :< ENumber 1,
--                   mkSpan "test" 1 5 1 6 :< ENumber 2
--                 ]
--           )
--   describe "object" $ do
--     it "{}" $
--       parseTest pExpr "test" "{}"
--         `shouldBe` Right
--           ( mkSpan "test" 1 1 1 3 :< EObject []
--           )
--     it "{\"a\": 1, \"b\": 2}" $
--       parseTest pExpr "test" "{\"a\": 1, \"b\": 2}"
--         `shouldBe` Right
--           ( mkSpan "test" 1 1 1 17
--               :< EObject
--                 [ ( mkSpan "test" 1 2 1 5 :< Identifier "a",
--                     mkSpan "test" 1 7 1 8 :< ENumber 1
--                   ),
--                   ( mkSpan "test" 1 10 1 13 :< Identifier "b",
--                     mkSpan "test" 1 15 1 16 :< ENumber 2
--                   )
--                 ]
--           )
--   describe "ref" $ do
--     it "a" $
--       parseTest pExpr "test" "a" `shouldBe` Right (mkSpan "test" 1 1 1 2 :< ERef (mkSpan "test" 1 1 1 2 :< Identifier "a"))
--     it "abc" $
--       parseTest pExpr "test" "abc" `shouldBe` Right (mkSpan "test" 1 1 1 4 :< ERef (mkSpan "test" 1 1 1 4 :< Identifier "abc"))
--     describe "app" $ do
--       it "(f 1)" $ do
--         parseTest pExpr "test" "(f 1)"
--           `shouldBe` Right
--             ( mkSpan "test" 1 1 1 6
--                 :< EApp
--                   Nothing
--                   ( mkSpan "test" 1 2 1 3
--                       :< ERef
--                         (mkSpan "test" 1 2 1 3 :< Identifier "f")
--                   )
--                   [(mkSpan "test" 1 4 1 5 :< ENumber 1, False)]
--             )
--     it "1 + 2" $ do
--       parseTest pExpr "test" "1 + 2"
--         `shouldBe` Right
--           ( mkSpan "test" 1 1 1 6
--               :< EApp
--                 Nothing
--                 ( mkSpan "test" 1 3 1 4
--                     :< ERef
--                       (mkSpan "test" 1 3 1 4 :< Identifier "+")
--                 )
--                 [ (mkSpan "test" 1 1 1 2 :< ENumber 1, False),
--                   (mkSpan "test" 1 5 1 6 :< ENumber 2, False)
--                 ]
--           )
--     it "(#alpine f 1 2)" $ do
--       parseTest pExpr "test" "(#alpine f 1 2)"
--         `shouldBe` Right
--           ( mkSpan "test" 1 1 1 16
--               :< EApp
--                 (Just (mkSpan "test" 1 2 1 9 :< Identifier "alpine"))
--                 ( mkSpan "test" 1 10 1 11
--                     :< ERef
--                       (mkSpan "test" 1 10 1 11 :< Identifier "f")
--                 )
--                 [ (mkSpan "test" 1 12 1 13 :< ENumber 1, False),
--                   (mkSpan "test" 1 14 1 15 :< ENumber 2, False)
--                 ]
--           )
