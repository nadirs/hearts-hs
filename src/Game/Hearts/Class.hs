{-# LANGUAGE FlexibleInstances #-}
module Game.Hearts.Class where

import Test.QuickCheck

-- Data types

data CardSuit = Hearts
              | Diamonds
              | Clubs
              | Spades
              deriving (Eq, Ord, Enum, Show)

data CardRank = Two
               | Three
               | Four
               | Five
               | Six
               | Seven
               | Eight
               | Nine
               | Ten
               | Jack
               | Queen
               | King
               | Ace
               deriving (Eq, Ord, Enum, Show)

data Card = Card { cardRank :: CardRank, cardSuit :: CardSuit } deriving (Eq, Ord, Show)

type Player = String

type Score = Int

-- Classes

class IsCard c where
    getCard :: c -> Card

class HasCards t where
    putCard :: (IsCard c, Ord c) => c -> t c -> t c
    getIsCards :: IsCard c => t c -> [c]
    getCards :: IsCard c => t c -> [Card]
    getCards = map getCard . getIsCards

-- Instances

instance IsCard Card where
    getCard = id

instance IsCard (a, Card) where
    getCard = snd

instance Arbitrary CardSuit where
    arbitrary = elements [Hearts ..]

instance Arbitrary CardRank where
    arbitrary = elements [Two ..]

instance Arbitrary Card where
    arbitrary = do
        r <- arbitrary
        s <- arbitrary
        return $ Card r s

-- Functions

cardScore :: Card -> Score
cardScore (Card _ Hearts) = 1
cardScore (Card Queen Spades) = 13
cardScore _ = 0
