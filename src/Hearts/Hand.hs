{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Hearts.Hand where

import Data.Monoid (Monoid)
import Data.Foldable (Foldable)
import Data.Set (Set)
import qualified Data.Set as S

import Hearts.Class

newtype Hand_ a = Hand { unHand :: Set a } deriving (Eq, Ord, Show, Monoid, Foldable)

type Hand = Hand_ (Player, Card)

-- Instances

instance HasCards Hand_ where
    getIsCards = handToList
    putCard = insert

-- Functions

handFromList :: Ord a => [a] -> Hand_ a
handFromList = Hand . S.fromList

handToList :: Hand_ a -> [a]
handToList = S.toList . unHand

insert :: Ord a => a -> Hand_ a -> Hand_ a
insert c = Hand . S.insert c . unHand

emptyHand :: Hand
emptyHand = Hand S.empty
