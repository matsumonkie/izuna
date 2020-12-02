module FooSpec where

import           Test.Hspec



spec :: Spec
spec =
  describe "foo" $
    it "bar" $
      True `shouldBe` False
