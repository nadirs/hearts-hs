module Hearts.Game where

import Hearts.Class
import Hearts.Hand

type Validation a r = (Player, Card) -> a -> r a

data PlayHandResult a = PlayHand (Player, Card) a
                      | ForbiddenPlay a
                      deriving (Eq, Show)

validatePlayOnHand :: Validation Hand PlayHandResult
validatePlayOnHand (p, c) h =
    case filter ((p ==) . fst) (handToList h) of
      [] -> PlayHand (p, c) h
      _ -> ForbiddenPlay h
