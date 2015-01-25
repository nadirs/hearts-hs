{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Hearts.Deck where

import Prelude hiding (foldr)
import Data.Monoid (Monoid)
import Data.Foldable (Foldable, foldr)
import Data.Set (Set)
import qualified Data.Set as S

import Test.QuickCheck

import Hearts.Class

-- Data types

newtype Deck_ a = Deck { unDeck :: Set a } deriving (Eq, Ord, Show, Monoid, Foldable)
type Deck = Deck_ Card


-- Instances

instance HasCards Deck_ where
    getIsCards = deckToList
    putCard = deckInsert

instance (Arbitrary a, Ord a) => Arbitrary (Deck_ a) where
    arbitrary = (Deck . S.fromList) `fmap` arbitrary


-- Functions

emptyDeck :: Deck_ a
emptyDeck = Deck S.empty

deckFromList :: Ord a => [a] -> Deck_ a
deckFromList = Deck . S.fromList

deckToList :: Deck_ a  -> [a]
deckToList = S.toList . unDeck

deckInsert :: Ord a => a -> Deck_ a -> Deck_ a
deckInsert c = Deck . S.insert c . unDeck

makeDeck :: Deck
makeDeck = deckFromList [ Card rank suit | suit <- [Hearts ..], rank <- [Two ..]]

deckScore :: Deck -> Score
deckScore deck = if allScoreCards == (allScoreCards `S.intersection` unDeck deck) then 0 else foldr (\card acc -> acc + cardScore card) 0 (unDeck deck)
  where
    allScoreCards = S.fromList $ Card Queen Spades : [Card rank Hearts | rank <- [Two ..]]

