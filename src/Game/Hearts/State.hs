module Game.Hearts.State where

import Control.Monad.Trans.State

import Game.Hearts.Deck
import Game.Hearts.Hand

import Game.Hearts.Class

data HeartsState = HeartsState
                 { turns :: [Hand]
                 , players :: [PlayerState]
                 } deriving (Eq, Show)

data PlayerState = PlayerState
                 { playerId :: Player
                 , playerHandDeck :: Deck
                 , playerStoreDeck :: Deck
                 } deriving (Eq, Show)

type HeartsGame = State HeartsState

-- Functions

initHeartsState :: HeartsState
initHeartsState = HeartsState [] []

initPlayer :: Player -> PlayerState
initPlayer p = PlayerState p emptyDeck emptyDeck
