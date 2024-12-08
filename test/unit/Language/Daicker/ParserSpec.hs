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
    it "image" $ do
      parse pExpr "test" "#alpine:3.12"
        `shouldBe` Right
          ( mkSpan "test" 1 1 1 13
              :< EImage (mkSpan "test" 1 1 1 13 :< Identifier "alpine:3.12"),
            [Token TKImage (mkSpan "test" 1 1 1 13)]
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
                (mkSpan "test" 1 9 1 10 :< EVar (mkSpan "test" 1 9 1 10 :< Identifier "a"))
                Nothing,
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
                (mkSpan "test" 1 13 1 14 :< EVar (mkSpan "test" 1 13 1 14 :< Identifier "a"))
                Nothing,
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
    it "lambda with rest parameter" $ do
      parse pExpr "test" "\\(a...) -> a"
        `shouldBe` Right
          ( mkSpan "test" 1 1 1 13
              :< ELambda
                [ mkSpan "test" 1 3 1 7
                    :< PositionedParameter
                      (mkSpan "test" 1 3 1 4 :< Identifier "a")
                      True
                      False
                      Nothing
                      Nothing
                ]
                (mkSpan "test" 1 12 1 13 :< EVar (mkSpan "test" 1 12 1 13 :< Identifier "a"))
                Nothing,
            [ Token TKSep (mkSpan "test" 1 1 1 2),
              Token TKSep (mkSpan "test" 1 2 1 3),
              Token TKParameter (mkSpan "test" 1 3 1 4),
              Token TKOp (mkSpan "test" 1 4 1 7),
              Token TKSep (mkSpan "test" 1 7 1 8),
              Token TKSep (mkSpan "test" 1 9 1 11),
              Token TKVar (mkSpan "test" 1 12 1 13)
            ]
          )
    it "lambda with optional parameter" $ do
      parse pExpr "test" "\\(a?) -> a"
        `shouldBe` Right
          ( mkSpan "test" 1 1 1 11
              :< ELambda
                [ mkSpan "test" 1 3 1 5
                    :< PositionedParameter
                      (mkSpan "test" 1 3 1 4 :< Identifier "a")
                      False
                      True
                      Nothing
                      Nothing
                ]
                (mkSpan "test" 1 10 1 11 :< EVar (mkSpan "test" 1 10 1 11 :< Identifier "a"))
                Nothing,
            [ Token TKSep (mkSpan "test" 1 1 1 2),
              Token TKSep (mkSpan "test" 1 2 1 3),
              Token TKParameter (mkSpan "test" 1 3 1 4),
              Token TKOp (mkSpan "test" 1 4 1 5),
              Token TKSep (mkSpan "test" 1 5 1 6),
              Token TKSep (mkSpan "test" 1 7 1 9),
              Token TKVar (mkSpan "test" 1 10 1 11)
            ]
          )
    it "lambda with typed parameter" $ do
      parse pExpr "test" "\\(a: Number) -> a"
        `shouldBe` Right
          ( mkSpan "test" 1 1 1 18
              :< ELambda
                [ mkSpan "test" 1 3 1 12
                    :< PositionedParameter
                      (mkSpan "test" 1 3 1 4 :< Identifier "a")
                      False
                      False
                      (Just (mkSpan "test" 1 6 1 12 :< TVar (mkSpan "test" 1 6 1 12 :< Identifier "Number")))
                      Nothing
                ]
                (mkSpan "test" 1 17 1 18 :< EVar (mkSpan "test" 1 17 1 18 :< Identifier "a"))
                Nothing,
            [ Token TKSep (mkSpan "test" 1 1 1 2),
              Token TKSep (mkSpan "test" 1 2 1 3),
              Token TKParameter (mkSpan "test" 1 3 1 4),
              Token TKSep (mkSpan "test" 1 4 1 5),
              Token TKTypeVar (mkSpan "test" 1 6 1 12),
              Token TKSep (mkSpan "test" 1 12 1 13),
              Token TKSep (mkSpan "test" 1 14 1 16),
              Token TKVar (mkSpan "test" 1 17 1 18)
            ]
          )
    it "lambda with return type" $ do
      parse pExpr "test" "\\(a): Number -> a"
        `shouldBe` Right
          ( mkSpan "test" 1 1 1 18
              :< ELambda
                [ mkSpan "test" 1 3 1 4
                    :< PositionedParameter
                      (mkSpan "test" 1 3 1 4 :< Identifier "a")
                      False
                      False
                      Nothing
                      Nothing
                ]
                (mkSpan "test" 1 17 1 18 :< EVar (mkSpan "test" 1 17 1 18 :< Identifier "a"))
                (Just (mkSpan "test" 1 7 1 13 :< TVar (mkSpan "test" 1 7 1 13 :< Identifier "Number"))),
            [ Token TKSep (mkSpan "test" 1 1 1 2),
              Token TKSep (mkSpan "test" 1 2 1 3),
              Token TKParameter (mkSpan "test" 1 3 1 4),
              Token TKSep (mkSpan "test" 1 4 1 5),
              Token TKSep (mkSpan "test" 1 5 1 6),
              Token TKTypeVar (mkSpan "test" 1 7 1 13),
              Token TKSep (mkSpan "test" 1 14 1 16),
              Token TKVar (mkSpan "test" 1 17 1 18)
            ]
          )
    it "call with positioned argument" $ do
      parse pExpr "test" "f(1)"
        `shouldBe` Right
          ( mkSpan "test" 1 1 1 5
              :< ECall
                (mkSpan "test" 1 1 1 2 :< EVar (mkSpan "test" 1 1 1 2 :< Identifier "f"))
                [mkSpan "test" 1 3 1 4 :< PositionedArgument False (mkSpan "test" 1 3 1 4 :< ENumber 1)],
            [ Token TKVar (mkSpan "test" 1 1 1 2),
              Token TKSep (mkSpan "test" 1 2 1 3),
              Token TKNumber (mkSpan "test" 1 3 1 4),
              Token TKSep (mkSpan "test" 1 4 1 5)
            ]
          )
    it "call with keyword argument" $ do
      parse pExpr "test" "f(a = 1)"
        `shouldBe` Right
          ( mkSpan "test" 1 1 1 9
              :< ECall
                (mkSpan "test" 1 1 1 2 :< EVar (mkSpan "test" 1 1 1 2 :< Identifier "f"))
                [ mkSpan "test" 1 3 1 8
                    :< KeywordArgument
                      (mkSpan "test" 1 3 1 4 :< Identifier "a")
                      (mkSpan "test" 1 7 1 8 :< ENumber 1)
                ],
            [ Token TKVar (mkSpan "test" 1 1 1 2),
              Token TKSep (mkSpan "test" 1 2 1 3),
              Token TKParameter (mkSpan "test" 1 3 1 4),
              Token TKSep (mkSpan "test" 1 5 1 6),
              Token TKNumber (mkSpan "test" 1 7 1 8),
              Token TKSep (mkSpan "test" 1 8 1 9)
            ]
          )
    it "call with variable positioned argument" $ do
      parse pExpr "test" "f(a)"
        `shouldBe` Right
          ( mkSpan "test" 1 1 1 5
              :< ECall
                (mkSpan "test" 1 1 1 2 :< EVar (mkSpan "test" 1 1 1 2 :< Identifier "f"))
                [mkSpan "test" 1 3 1 4 :< PositionedArgument False (mkSpan "test" 1 3 1 4 :< EVar (mkSpan "test" 1 3 1 4 :< Identifier "a"))],
            [ Token TKVar (mkSpan "test" 1 1 1 2),
              Token TKSep (mkSpan "test" 1 2 1 3),
              Token TKVar (mkSpan "test" 1 3 1 4),
              Token TKSep (mkSpan "test" 1 4 1 5)
            ]
          )
    it "call with positioned and keyword argument" $ do
      parse pExpr "test" "f(1, a = 2)"
        `shouldBe` Right
          ( mkSpan "test" 1 1 1 12
              :< ECall
                (mkSpan "test" 1 1 1 2 :< EVar (mkSpan "test" 1 1 1 2 :< Identifier "f"))
                [ mkSpan "test" 1 3 1 4
                    :< PositionedArgument False (mkSpan "test" 1 3 1 4 :< ENumber 1),
                  mkSpan "test" 1 6 1 11
                    :< KeywordArgument
                      (mkSpan "test" 1 6 1 7 :< Identifier "a")
                      (mkSpan "test" 1 10 1 11 :< ENumber 2)
                ],
            [ Token TKVar (mkSpan "test" 1 1 1 2),
              Token TKSep (mkSpan "test" 1 2 1 3),
              Token TKNumber (mkSpan "test" 1 3 1 4),
              Token TKSep (mkSpan "test" 1 4 1 5),
              Token TKParameter (mkSpan "test" 1 6 1 7),
              Token TKSep (mkSpan "test" 1 8 1 9),
              Token TKNumber (mkSpan "test" 1 10 1 11),
              Token TKSep (mkSpan "test" 1 11 1 12)
            ]
          )
    it "dot accessor" $ do
      parse pExpr "test" "a.b"
        `shouldBe` Right
          ( mkSpan "test" 1 1 1 4
              :< EAccessor
                (mkSpan "test" 1 1 1 2 :< EVar (mkSpan "test" 1 1 1 2 :< Identifier "a"))
                (mkSpan "test" 1 3 1 4 :< EString "b"),
            [ Token TKVar (mkSpan "test" 1 1 1 2),
              Token TKSep (mkSpan "test" 1 2 1 3),
              Token TKProperty (mkSpan "test" 1 3 1 4)
            ]
          )
    it "bracket accessor" $ do
      parse pExpr "test" "a[\"b\"]"
        `shouldBe` Right
          ( mkSpan "test" 1 1 1 7
              :< EAccessor
                (mkSpan "test" 1 1 1 2 :< EVar (mkSpan "test" 1 1 1 2 :< Identifier "a"))
                (mkSpan "test" 1 3 1 6 :< EString "b"),
            [ Token TKVar (mkSpan "test" 1 1 1 2),
              Token TKSep (mkSpan "test" 1 2 1 3),
              Token TKString (mkSpan "test" 1 3 1 6),
              Token TKSep (mkSpan "test" 1 6 1 7)
            ]
          )
    it "binary expr" $ do
      parse pExpr "test" "1 + 2"
        `shouldBe` Right
          ( mkSpan "test" 1 1 1 6
              :< ECall
                (mkSpan "test" 1 3 1 4 :< EVar (mkSpan "test" 1 3 1 4 :< Identifier "+"))
                [ mkSpan "test" 1 1 1 2 :< PositionedArgument False (mkSpan "test" 1 1 1 2 :< ENumber 1),
                  mkSpan "test" 1 5 1 6 :< PositionedArgument False (mkSpan "test" 1 5 1 6 :< ENumber 2)
                ],
            [ Token TKNumber (mkSpan "test" 1 1 1 2),
              Token TKOp (mkSpan "test" 1 3 1 4),
              Token TKNumber (mkSpan "test" 1 5 1 6)
            ]
          )
    it "command sugar syntax" $ do
      parse pExpr "test" "$ echo hello;"
        `shouldBe` Right
          ( mkSpan "test" 1 1 1 14
              :< ECall
                (mkSpan "test" 1 1 1 2 :< EVar (mkSpan "test" 1 1 1 2 :< Identifier "$"))
                [ mkSpan "test" 1 3 1 13
                    :< PositionedArgument False (mkSpan "test" 1 3 1 13 :< EString "echo hello")
                ],
            [ Token TKVar (mkSpan "test" 1 1 1 2),
              Token TKString (mkSpan "test" 1 3 1 13),
              Token TKSep (mkSpan "test" 1 13 1 14)
            ]
          )
    it "command sugar syntax with image tag" $ do
      parse pExpr "test" "$[#alpine:3.12] echo hello;"
        `shouldBe` Right
          ( mkSpan "test" 1 1 1 28
              :< ECall
                (mkSpan "test" 1 1 1 2 :< EVar (mkSpan "test" 1 1 1 2 :< Identifier "$"))
                [ mkSpan "test" 1 2 1 16
                    :< KeywordArgument
                      (mkSpan "test" 1 2 1 16 :< Identifier "image")
                      (mkSpan "test" 1 3 1 15 :< EImage (mkSpan "test" 1 3 1 15 :< Identifier "alpine:3.12")),
                  mkSpan "test" 1 17 1 27
                    :< PositionedArgument False (mkSpan "test" 1 17 1 27 :< EString "echo hello")
                ],
            [ Token TKVar (mkSpan "test" 1 1 1 2),
              Token TKSep (mkSpan "test" 1 2 1 3),
              Token TKImage (mkSpan "test" 1 3 1 15),
              Token TKSep (mkSpan "test" 1 15 1 16),
              Token TKString (mkSpan "test" 1 17 1 27),
              Token TKSep (mkSpan "test" 1 27 1 28)
            ]
          )
  describe "type" $ do
    it "type variable" $ do
      parse pType "test" "Void"
        `shouldBe` Right
          ( mkSpan "test" 1 1 1 5
              :< TVar (mkSpan "test" 1 1 1 5 :< Identifier "Void"),
            [Token TKTypeVar (mkSpan "test" 1 1 1 5)]
          )
    it "null literal type" $ do
      parse pType "test" "null"
        `shouldBe` Right
          ( mkSpan "test" 1 1 1 5
              :< TNullLiteral,
            [Token TKNull (mkSpan "test" 1 1 1 5)]
          )
    it "boolean type" $ do
      parse pType "test" "true"
        `shouldBe` Right
          ( mkSpan "test" 1 1 1 5
              :< TBoolLiteral True,
            [Token TKBool (mkSpan "test" 1 1 1 5)]
          )
    it "number type" $ do
      parse pType "test" "1"
        `shouldBe` Right
          ( mkSpan "test" 1 1 1 2
              :< TNumberLiteral 1,
            [Token TKNumber (mkSpan "test" 1 1 1 2)]
          )
    it "string type" $ do
      parse pType "test" "\"abc\""
        `shouldBe` Right
          ( mkSpan "test" 1 1 1 6
              :< TStringLiteral "abc",
            [Token TKString (mkSpan "test" 1 1 1 6)]
          )
    it "array type" $ do
      parse pType "test" "[String]"
        `shouldBe` Right
          ( mkSpan "test" 1 1 1 9
              :< TParameterized
                (mkSpan "test" 1 1 1 9 :< TVar (mkSpan "test" 1 1 1 9 :< Identifier "Array"))
                [mkSpan "test" 1 2 1 8 :< TVar (mkSpan "test" 1 2 1 8 :< Identifier "String")],
            [ Token TKSep (mkSpan "test" 1 1 1 2),
              Token TKTypeVar (mkSpan "test" 1 2 1 8),
              Token TKSep (mkSpan "test" 1 8 1 9)
            ]
          )
    it "tuple type" $ do
      parse pType "test" "[String, Number]"
        `shouldBe` Right
          ( mkSpan "test" 1 1 1 17
              :< TParameterized
                (mkSpan "test" 1 1 1 17 :< TVar (mkSpan "test" 1 1 1 17 :< Identifier "Tuple"))
                [ mkSpan "test" 1 2 1 8 :< TVar (mkSpan "test" 1 2 1 8 :< Identifier "String"),
                  mkSpan "test" 1 10 1 16 :< TVar (mkSpan "test" 1 10 1 16 :< Identifier "Number")
                ],
            [ Token TKSep (mkSpan "test" 1 1 1 2),
              Token TKTypeVar (mkSpan "test" 1 2 1 8),
              Token TKSep (mkSpan "test" 1 8 1 9),
              Token TKTypeVar (mkSpan "test" 1 10 1 16),
              Token TKSep (mkSpan "test" 1 16 1 17)
            ]
          )
    it "object type" $ do
      parse pType "test" "{\"a\": String, \"b\": Number}"
        `shouldBe` Right
          ( mkSpan "test" 1 1 1 27
              :< TObject
                [ ( mkSpan "test" 1 2 1 5 :< TStringLiteral "a",
                    mkSpan "test" 1 7 1 13 :< TVar (mkSpan "test" 1 7 1 13 :< Identifier "String")
                  ),
                  ( mkSpan "test" 1 15 1 18 :< TStringLiteral "b",
                    mkSpan "test" 1 20 1 26 :< TVar (mkSpan "test" 1 20 1 26 :< Identifier "Number")
                  )
                ],
            [ Token TKSep (mkSpan "test" 1 1 1 2),
              Token TKString (mkSpan "test" 1 2 1 5),
              Token TKSep (mkSpan "test" 1 5 1 6),
              Token TKTypeVar (mkSpan "test" 1 7 1 13),
              Token TKSep (mkSpan "test" 1 13 1 14),
              Token TKString (mkSpan "test" 1 15 1 18),
              Token TKSep (mkSpan "test" 1 18 1 19),
              Token TKTypeVar (mkSpan "test" 1 20 1 26),
              Token TKSep (mkSpan "test" 1 26 1 27)
            ]
          )
    it "union type" $ do
      parse pType "test" "String | Number"
        `shouldBe` Right
          ( mkSpan "test" 1 1 1 16
              :< TParameterized
                (mkSpan "test" 1 8 1 9 :< TVar (mkSpan "test" 1 8 1 9 :< Identifier "|"))
                [ mkSpan "test" 1 1 1 7 :< TVar (mkSpan "test" 1 1 1 7 :< Identifier "String"),
                  mkSpan "test" 1 10 1 16 :< TVar (mkSpan "test" 1 10 1 16 :< Identifier "Number")
                ],
            [ Token TKTypeVar (mkSpan "test" 1 1 1 7),
              Token TKOp (mkSpan "test" 1 8 1 9),
              Token TKTypeVar (mkSpan "test" 1 10 1 16)
            ]
          )
  describe "expression statement" $ do
    it "variable" $ do
      parse pSExpr "test" "a = 1"
        `shouldBe` Right
          ( mkSpan "test" 1 1 1 6
              :< SExpr
                (mkSpan "test" 1 1 1 2 :< Identifier "a")
                (mkSpan "test" 1 5 1 6 :< ENumber 1),
            [ Token TKFunction (mkSpan "test" 1 1 1 2),
              Token TKSep (mkSpan "test" 1 3 1 4),
              Token TKNumber (mkSpan "test" 1 5 1 6)
            ]
          )
    it "function" $ do
      parse pSExpr "test" "f(a) = a"
        `shouldBe` Right
          ( mkSpan "test" 1 1 1 9
              :< SExpr
                (mkSpan "test" 1 1 1 2 :< Identifier "f")
                ( mkSpan "test" 1 2 1 9
                    :< ELambda
                      [ mkSpan "test" 1 3 1 4
                          :< PositionedParameter
                            (mkSpan "test" 1 3 1 4 :< Identifier "a")
                            False
                            False
                            Nothing
                            Nothing
                      ]
                      (mkSpan "test" 1 8 1 9 :< EVar (mkSpan "test" 1 8 1 9 :< Identifier "a"))
                      Nothing
                ),
            [ Token TKFunction (mkSpan "test" 1 1 1 2),
              Token TKSep (mkSpan "test" 1 2 1 3),
              Token TKParameter (mkSpan "test" 1 3 1 4),
              Token TKSep (mkSpan "test" 1 4 1 5),
              Token TKSep (mkSpan "test" 1 6 1 7),
              Token TKVar (mkSpan "test" 1 8 1 9)
            ]
          )
  describe "type statement" $ do
    it "type variable" $ do
      parse pSType "test" "type Name = String"
        `shouldBe` Right
          ( mkSpan "test" 1 1 1 19
              :< SType
                (mkSpan "test" 1 6 1 10 :< Identifier "Name")
                (mkSpan "test" 1 13 1 19 :< TVar (mkSpan "test" 1 13 1 19 :< Identifier "String")),
            [ Token TKKeyword (mkSpan "test" 1 1 1 5),
              Token TKTypeVar (mkSpan "test" 1 6 1 10),
              Token TKSep (mkSpan "test" 1 11 1 12),
              Token TKTypeVar (mkSpan "test" 1 13 1 19)
            ]
          )
    it "type variable with type parameter" $ do
      parse pSType "test" "type Names[T] = Array[T]"
        `shouldBe` Right
          ( mkSpan "test" 1 1 1 25
              :< SType
                (mkSpan "test" 1 6 1 11 :< Identifier "Names")
                ( mkSpan "test" 1 11 1 25
                    :< TFunc
                      [mkSpan "test" 1 12 1 13 :< Identifier "T"]
                      ( mkSpan "test" 1 17 1 25
                          :< TParameterized
                            (mkSpan "test" 1 17 1 22 :< TVar (mkSpan "test" 1 17 1 22 :< Identifier "Array"))
                            [mkSpan "test" 1 23 1 24 :< TVar (mkSpan "test" 1 23 1 24 :< Identifier "T")]
                      )
                ),
            [ Token TKKeyword (mkSpan "test" 1 1 1 5),
              Token TKTypeVar (mkSpan "test" 1 6 1 11),
              Token TKSep (mkSpan "test" 1 11 1 12),
              Token TKTypeParameter (mkSpan "test" 1 12 1 13),
              Token TKSep (mkSpan "test" 1 13 1 14),
              Token TKSep (mkSpan "test" 1 15 1 16),
              Token TKTypeVar (mkSpan "test" 1 17 1 22),
              Token TKSep (mkSpan "test" 1 22 1 23),
              Token TKTypeVar (mkSpan "test" 1 23 1 24),
              Token TKSep (mkSpan "test" 1 24 1 25)
            ]
          )
  describe "export" $ do
    it "expression export" $ do
      parse pExport "test" "export { a }"
        `shouldBe` Right
          ( mkSpan "test" 1 1 1 13
              :< Export
                [mkSpan "test" 1 10 1 11 :< Identifier "a"],
            [ Token TKKeyword (mkSpan "test" 1 1 1 7),
              Token TKSep (mkSpan "test" 1 8 1 9),
              Token TKVar (mkSpan "test" 1 10 1 11),
              Token TKSep (mkSpan "test" 1 12 1 13)
            ]
          )
    it "type export" $ do
      parse pExport "test" "export { Name }"
        `shouldBe` Right
          ( mkSpan "test" 1 1 1 16
              :< Export
                [mkSpan "test" 1 10 1 14 :< Identifier "Name"],
            [ Token TKKeyword (mkSpan "test" 1 1 1 7),
              Token TKSep (mkSpan "test" 1 8 1 9),
              Token TKTypeVar (mkSpan "test" 1 10 1 14),
              Token TKSep (mkSpan "test" 1 15 1 16)
            ]
          )
    it "multiple export" $ do
      parse pExport "test" "export { a, b }"
        `shouldBe` Right
          ( mkSpan "test" 1 1 1 16
              :< Export
                [mkSpan "test" 1 10 1 11 :< Identifier "a", mkSpan "test" 1 13 1 14 :< Identifier "b"],
            [ Token TKKeyword (mkSpan "test" 1 1 1 7),
              Token TKSep (mkSpan "test" 1 8 1 9),
              Token TKVar (mkSpan "test" 1 10 1 11),
              Token TKSep (mkSpan "test" 1 11 1 12),
              Token TKVar (mkSpan "test" 1 13 1 14),
              Token TKSep (mkSpan "test" 1 15 1 16)
            ]
          )
  describe "import" $ do
    it "wildcard import" $ do
      parse pImport "test" "import * from \"test.daic\""
        `shouldBe` Right
          ( mkSpan "test" 1 1 1 26
              :< Import
                (mkSpan "test" 1 8 1 9 :< FullScope)
                Nothing
                (mkSpan "test" 1 15 1 26 :< LocalFile "test.daic"),
            [ Token TKKeyword (mkSpan "test" 1 1 1 7),
              Token TKOp (mkSpan "test" 1 8 1 9),
              Token TKKeyword (mkSpan "test" 1 10 1 14),
              Token TKString (mkSpan "test" 1 15 1 26)
            ]
          )
    it "partial import" $ do
      parse pImport "test" "import {a, b} from \"test.daic\""
        `shouldBe` Right
          ( mkSpan "test" 1 1 1 31
              :< Import
                (mkSpan "test" 1 8 1 14 :< PartialScope [mkSpan "test" 1 9 1 10 :< Identifier "a", mkSpan "test" 1 12 1 13 :< Identifier "b"])
                Nothing
                (mkSpan "test" 1 20 1 31 :< LocalFile "test.daic"),
            [ Token TKKeyword (mkSpan "test" 1 1 1 7),
              Token TKSep (mkSpan "test" 1 8 1 9),
              Token TKVar (mkSpan "test" 1 9 1 10),
              Token TKSep (mkSpan "test" 1 10 1 11),
              Token TKVar (mkSpan "test" 1 12 1 13),
              Token TKSep (mkSpan "test" 1 13 1 14),
              Token TKKeyword (mkSpan "test" 1 15 1 19),
              Token TKString (mkSpan "test" 1 20 1 31)
            ]
          )
  describe "module" $ do
    it "simple module" $ do
      parse pModule "test" "import { g } from \"test.daic\"\nexport { f }\nf(a) = h(a)\nh(a) = g(a)\n"
        `shouldBe` Right
          ( mkSpan "test" 1 1 5 1
              :< Module
                [ mkSpan "test" 1 1 1 30
                    :< Import
                      (mkSpan "test" 1 8 1 13 :< PartialScope [mkSpan "test" 1 10 1 11 :< Identifier "g"])
                      Nothing
                      (mkSpan "test" 1 19 1 30 :< LocalFile "test.daic")
                ]
                ( Just $
                    mkSpan "test" 2 1 2 13
                      :< Export
                        [mkSpan "test" 2 10 2 11 :< Identifier "f"]
                )
                [ mkSpan "test" 3 1 3 12
                    :< SExpr
                      (mkSpan "test" 3 1 3 2 :< Identifier "f")
                      ( mkSpan "test" 3 2 3 12
                          :< ELambda
                            [ mkSpan "test" 3 3 3 4
                                :< PositionedParameter
                                  (mkSpan "test" 3 3 3 4 :< Identifier "a")
                                  False
                                  False
                                  Nothing
                                  Nothing
                            ]
                            ( mkSpan "test" 3 8 3 12
                                :< ECall
                                  (mkSpan "test" 3 8 3 9 :< EVar (mkSpan "test" 3 8 3 9 :< Identifier "h"))
                                  [mkSpan "test" 3 10 3 11 :< PositionedArgument False (mkSpan "test" 3 10 3 11 :< EVar (mkSpan "test" 3 10 3 11 :< Identifier "a"))]
                            )
                            Nothing
                      ),
                  mkSpan "test" 4 1 4 12
                    :< SExpr
                      (mkSpan "test" 4 1 4 2 :< Identifier "h")
                      ( mkSpan "test" 4 2 4 12
                          :< ELambda
                            [ mkSpan "test" 4 3 4 4
                                :< PositionedParameter
                                  (mkSpan "test" 4 3 4 4 :< Identifier "a")
                                  False
                                  False
                                  Nothing
                                  Nothing
                            ]
                            ( mkSpan "test" 4 8 4 12
                                :< ECall
                                  (mkSpan "test" 4 8 4 9 :< EVar (mkSpan "test" 4 8 4 9 :< Identifier "g"))
                                  [mkSpan "test" 4 10 4 11 :< PositionedArgument False (mkSpan "test" 4 10 4 11 :< EVar (mkSpan "test" 4 10 4 11 :< Identifier "a"))]
                            )
                            Nothing
                      )
                ],
            [ Token TKKeyword (mkSpan "test" 1 1 1 7),
              Token TKSep (mkSpan "test" 1 8 1 9),
              Token TKVar (mkSpan "test" 1 10 1 11),
              Token TKSep (mkSpan "test" 1 12 1 13),
              Token TKKeyword (mkSpan "test" 1 14 1 18),
              Token TKString (mkSpan "test" 1 19 1 30),
              Token TKKeyword (mkSpan "test" 2 1 2 7),
              Token TKSep (mkSpan "test" 2 8 2 9),
              Token TKVar (mkSpan "test" 2 10 2 11),
              Token TKSep (mkSpan "test" 2 12 2 13),
              Token TKFunction (mkSpan "test" 3 1 3 2),
              Token TKSep (mkSpan "test" 3 2 3 3),
              Token TKParameter (mkSpan "test" 3 3 3 4),
              Token TKSep (mkSpan "test" 3 4 3 5),
              Token TKSep (mkSpan "test" 3 6 3 7),
              Token TKVar (mkSpan "test" 3 8 3 9),
              Token TKSep (mkSpan "test" 3 9 3 10),
              Token TKVar (mkSpan "test" 3 10 3 11),
              Token TKSep (mkSpan "test" 3 11 3 12),
              Token TKFunction (mkSpan "test" 4 1 4 2),
              Token TKSep (mkSpan "test" 4 2 4 3),
              Token TKParameter (mkSpan "test" 4 3 4 4),
              Token TKSep (mkSpan "test" 4 4 4 5),
              Token TKSep (mkSpan "test" 4 6 4 7),
              Token TKVar (mkSpan "test" 4 8 4 9),
              Token TKSep (mkSpan "test" 4 9 4 10),
              Token TKVar (mkSpan "test" 4 10 4 11),
              Token TKSep (mkSpan "test" 4 11 4 12)
            ]
          )
