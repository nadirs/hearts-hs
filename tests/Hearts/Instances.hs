module Hearts.Instances where

import Test.QuickCheck

import Hearts

instance Arbitrary CardSuit where
    arbitrary = elements [Hearts ..]

instance Arbitrary CardRank where
    arbitrary = elements [Two ..]

instance Arbitrary Card where
    arbitrary = do
        r <- arbitrary
        s <- arbitrary
        return $ Card r s

instance (Arbitrary a, Ord a) => Arbitrary (Deck_ a) where
    arbitrary = (deckFromList) `fmap` arbitrary

