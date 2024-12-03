module Language.Daicker.BundlerSpec (spec) where

import Control.Comonad.Cofree
import Language.Daicker.AST
import Language.Daicker.Bundler
import Test.Hspec

spec :: Spec
spec = do
  describe "findExpr" $ do
    it "find expr in prelude module" $ do
      findExpr env "prelude-f1" `shouldBe` Just (env, "prelude-f1" :< ENull)
    it "find expr in current module" $ do
      findExpr env "current-f1" `shouldBe` Just (env, "current-f1" :< ENull)
    it "find expr in dependency module" $ do
      findExpr env "mod1-f1" `shouldBe` Just (env1Stub, "mod1-f1" :< ENull)
    it "find argument" $ do
      findExpr env "arg1" `shouldBe` Just (env, "arg1" :< ENull)
    it "find nothing" $ do
      findExpr env "not-found" `shouldBe` Nothing
    it "find internal expr" $ do
      findExpr env "mod1-f2" `shouldBe` Nothing
    it "find partial import expr" $ do
      findExpr env "mod2-f1" `shouldBe` Just (env2Stub, "mod2-f1" :< ENull)
    it "find unimported expr" $ do
      findExpr env "mod2-f2" `shouldBe` Nothing

env :: Environment String
env =
  Environment
    { preludeModule = preludeStub,
      currentModule = currentStub,
      dependencyModules =
        [ ("mod1" :< LocalFile "mod1.daic", env1Stub),
          ("mod2" :< LocalFile "mod2.daic", env2Stub)
        ],
      packedArguments = [("arg1", ("arg1" :< ENull, Just $ "type-arg1" :< TNullLiteral))]
    }

preludeStub :: Module String
preludeStub =
  "preludeStub"
    :< Module
      []
      Nothing
      [ "f1" :< SExpr ("prelude-f1" :< Identifier "prelude-f1") ("prelude-f1" :< ENull)
      ]

currentStub :: Module String
currentStub =
  "currentStub"
    :< Module
      [ "import" :< Import ("mod1-scope" :< FullScope) Nothing ("mod1" :< LocalFile "mod1.daic"),
        "import"
          :< Import
            ( "mod2-scope"
                :< PartialScope
                  ["mod2-f1-import" :< Identifier "mod2-f1"]
            )
            Nothing
            ("mod2" :< LocalFile "mod2.daic")
      ]
      Nothing
      [ "f1" :< SExpr ("current-f1" :< Identifier "current-f1") ("current-f1" :< ENull)
      ]

env1Stub :: Environment String
env1Stub =
  Environment
    { preludeModule = preludeStub,
      currentModule = mod1Stub,
      dependencyModules = [],
      packedArguments = []
    }

mod1Stub :: Module String
mod1Stub =
  "mod1Stub"
    :< Module
      []
      (Just $ "export" :< Export ["export-mod1-f1" :< Identifier "mod1-f1"])
      [ "f1" :< SExpr ("mod1-f1" :< Identifier "mod1-f1") ("mod1-f1" :< ENull),
        "f2" :< SExpr ("mod1-f2" :< Identifier "mod1-f2") ("mod1-f2" :< ENull)
      ]

env2Stub :: Environment String
env2Stub =
  Environment
    { preludeModule = preludeStub,
      currentModule = mod2Stub,
      dependencyModules = [],
      packedArguments = []
    }

mod2Stub :: Module String
mod2Stub =
  "mod1Stub"
    :< Module
      []
      Nothing
      [ "f1" :< SExpr ("mod2-f1" :< Identifier "mod2-f1") ("mod2-f1" :< ENull),
        "f2" :< SExpr ("mod2-f2" :< Identifier "mod2-f2") ("mod2-f2" :< ENull)
      ]
