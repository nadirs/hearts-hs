{-# LANGUAGE GeneralizedNewtypeDeriving,
    TypeSynonymInstances,
    FlexibleInstances #-}
module Game.Hearts where

import Prelude hiding (foldr)

import Data.Monoid (Monoid)
import Data.Foldable (Foldable, foldr)
import Data.Set (Set)
import qualified Data.Set as S

import Control.Monad.Trans.State
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

data HeartsState = HeartsState
                 { turns :: [Hand]
                 , players :: [PlayerState]
                 } deriving (Eq, Show)

data PlayerState = PlayerState
                 { playerId :: Player
                 , playerHandDeck :: Deck
                 , playerStoreDeck :: Deck
                 } deriving (Eq, Show)

newtype Hand_ a = Hand { unHand :: Set a } deriving (Eq, Ord, Show, Monoid, Foldable)
type Hand = Hand_ (Player, Card)

newtype Deck_ a = Deck { unDeck :: Set a } deriving (Eq, Ord, Show, Monoid, Foldable)
type Deck = Deck_ Card

type Player = String
type Score = Int
type HeartsGame = State HeartsState

-- classes
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

instance HasCards Deck_ where
    getIsCards = deckToList
    putCard = deckInsert

instance HasCards Hand_ where
    getIsCards = handToList
    putCard = handInsert

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

handFromList :: Ord a => [a] -> Hand_ a
handFromList = Hand . S.fromList

handToList :: Hand_ a -> [a]
handToList = S.toList . unHand

handInsert :: Ord a => a -> Hand_ a -> Hand_ a
handInsert c = Hand . S.insert c . unHand

makeDeck :: Deck
makeDeck = deckFromList [ Card rank suit | suit <- [Hearts ..], rank <- [Two ..]]

cardScore :: Card -> Score
cardScore (Card _ Hearts) = 1
cardScore (Card Queen Spades) = 13
cardScore _ = 0

deckScore :: Deck -> Score
deckScore deck = if allScoreCards == (allScoreCards `S.intersection` unDeck deck) then 0 else foldr (\card acc -> acc + cardScore card) 0 (unDeck deck)
  where
    allScoreCards = S.fromList $ Card Queen Spades : [Card rank Hearts | rank <- [Two ..]]

emptyHand :: Hand
emptyHand = Hand S.empty

playHand :: (Player, Card) -> Hand -> Hand
playHand = putCard

initHeartsState :: HeartsState
initHeartsState = HeartsState [] []

initPlayer :: Player -> PlayerState
initPlayer p = PlayerState p emptyDeck emptyDeck
