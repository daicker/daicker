module Language.Daicker.ExecutorSpec (spec) where

import Control.Comonad.Cofree
import Control.Monad.Except (runExceptT)
import GHC.Base (returnIO)
import Language.Daicker.AST
import Language.Daicker.Bundler (Environment (..))
import Language.Daicker.Executor (eval)
import Language.Daicker.Span (Span, mkSpan)
import Language.Daicker.StdLib (prelude)
import Test.Hspec

spec :: Spec
spec = do
  describe "exec" $ do
    it "null" $ do
      result <- runExceptT $ eval emptyEnv (1 :< ENull)
      result `shouldBe` Right (1 :< ENull)
    it "bool" $ do
      result <- runExceptT $ eval emptyEnv (1 :< EBool True)
      result `shouldBe` Right (1 :< EBool True)
    it "number" $ do
      result <- runExceptT $ eval emptyEnv (1 :< ENumber 42)
      result `shouldBe` Right (1 :< ENumber 42)
    it "string" $ do
      result <- runExceptT $ eval emptyEnv (1 :< EString "hello")
      result `shouldBe` Right (1 :< EString "hello")
    it "array" $ do
      result <-
        runExceptT $
          eval emptyEnv (1 :< EArray [2 :< ENumber 1, 3 :< ENumber 2])
      result `shouldBe` Right (1 :< EArray [2 :< ENumber 1, 3 :< ENumber 2])
    it "object" $ do
      result <-
        runExceptT $
          eval
            emptyEnv
            ( 1
                :< EObject
                  [ (2 :< EString "a", 3 :< ENumber 1),
                    (4 :< EString "b", 5 :< ENumber 2)
                  ]
            )
      result
        `shouldBe` Right
          ( 1
              :< EObject
                [ (2 :< EString "a", 3 :< ENumber 1),
                  (4 :< EString "b", 5 :< ENumber 2)
                ]
          )
    it "image" $ do
      result <- runExceptT $ eval emptyEnv (1 :< EImage (2 :< Identifier "image"))
      result `shouldBe` Right (1 :< EImage (2 :< Identifier "image"))
    it "var" $ do
      result <- runExceptT $ eval emptyEnv (1 :< EVar (2 :< Identifier "empty"))
      result `shouldBe` Right (0 :< ENull)

emptyEnv :: Environment Integer
emptyEnv =
  Environment
    { preludeModule = emptyModule,
      currentModule = emptyModule,
      dependencyModules = [],
      packedArguments = []
    }

emptyModule :: Module Integer
emptyModule =
  0
    :< Module
      []
      Nothing
      [ 0 :< SExpr (0 :< Identifier "empty") (0 :< ENull)
      ]

instance Semigroup Integer where
  (<>) = (+)
