module HeartsSpec (spec) where

import Test.Hspec
import Test.Hspec.QuickCheck

spec :: Spec
spec = describe "base test" $ do
        it "matches True values" $
            True `shouldBe` True

        prop "The () type is inhabited by the unit value" $
            \ x -> () == x
