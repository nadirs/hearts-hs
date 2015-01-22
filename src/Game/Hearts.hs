{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Game.Hearts where

import Prelude hiding (foldr)
import Data.Monoid (Monoid)
import Data.Foldable
import Data.Set (Set)
import qualified Data.Set as S

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

type Score = Int

newtype Deck' a = Deck { unDeck :: Set a } deriving (Eq, Ord, Show, Monoid,Foldable)
type Deck = Deck' Card


-- Instances
instance Arbitrary CardSuit where
    arbitrary = elements [Hearts ..]

instance Arbitrary CardRank where
    arbitrary = elements [Two ..]

instance Arbitrary Card where
    arbitrary = do
        r <- arbitrary
        s <- arbitrary
        return $ Card r s

instance (Arbitrary a, Ord a) => Arbitrary (Deck' a) where
    arbitrary = (Deck . S.fromList) `fmap` arbitrary


-- Functions
fromList :: [Card] -> Deck
fromList = Deck . S.fromList

toList :: Deck -> [Card]
toList = S.toList . unDeck

makeDeck :: Deck
makeDeck = fromList [ Card rank suit | suit <- [Hearts ..], rank <- [Two ..]]

cardScore :: Card -> Score
cardScore (Card _ Hearts) = 1
cardScore (Card Queen Spades) = 13
cardScore _ = 0

deckScore :: Deck -> Score
deckScore deck = if allScoreCards == (allScoreCards `S.intersection` unDeck deck) then 0 else foldr (\card acc -> acc + cardScore card) 0 deck
  where
    allScoreCards = S.fromList $ Card Queen Spades : [Card rank Hearts | rank <- [Two ..]]
