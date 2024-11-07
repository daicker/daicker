module Language.Daicker.ExecutorSpec (spec) where

import Control.Comonad.Cofree
import Control.Monad.Except (runExceptT)
import GHC.Base (returnIO)
import Language.Daicker.AST
import Language.Daicker.Bundler (Bundle (Bundle))
import Language.Daicker.Executor (eval)
import Language.Daicker.Span (Span, mkSpan)
import Language.Daicker.StdLib (prelude)
import Test.Hspec

spec :: Spec
spec = do
  describe "exec" $ do
    it "null" $ do
      result <- runExceptT $ eval emptyBundle (() :< ENull)
      result `shouldBe` Right (() :< ENull)
    it "bool" $ do
      result <- runExceptT $ eval emptyBundle (() :< EBool True)
      result `shouldBe` Right (() :< EBool True)

emptyBundle :: Bundle ()
emptyBundle = Bundle emptyModule [] emptyModule [] []

emptyModule :: Module ()
emptyModule = () :< Module [] Nothing []
