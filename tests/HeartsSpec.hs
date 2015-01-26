module HeartsSpec (spec) where

import Prelude hiding (foldr)
import qualified Data.Foldable as F
import Data.List (sort, group)
import qualified Data.Set as S

import Test.Hspec
import Test.Hspec.QuickCheck

import Hearts
import Hearts.Instances()

{-# ANN module "HLint: ignore Redundant do" #-}

spec :: Spec
spec = parallel $ do
    describe "Card" $ do
        it "can be ordered by rank" $ do
            Card Ace Hearts `shouldSatisfy` (> Card King Hearts)
            Card Five Spades `shouldSatisfy` (< Card Jack Spades)

        context "when counting points" $ do
            context "when is not a Heart or the Queen of Spades" $ do
                prop "is worth zero points" $ do
                    \c -> case c of
                        Card _ Clubs -> cardScore c == 0
                        Card _ Diamonds -> cardScore c == 0
                        _ -> otherwise

            context "when is a Heart" $ do
                prop "is worth one point" $ do
                    \c -> case c of
                        Card _ Hearts -> cardScore c == 1
                        _ -> otherwise

            context "when is the Queen of Spades" $ do
                prop "is worth 13 points" $ do
                    \c -> case c of
                        Card Queen Spades -> cardScore c == 13
                        _ -> otherwise


    describe "Deck" $ do
        it "contains no duplicates" $ do
            let deck = makeDeck
            let isUnique [_] = True
                isUnique _ = False
                keepDuplicates = filter (not . isUnique)
            (keepDuplicates . group . sort . deckToList) deck `shouldBe` []

        it "can be reduced to its card count" $
            F.foldr (const succ) 0 makeDeck `shouldBe` (52 :: Int)


    describe "deckScore" $ do
        it "counts how many points a deck is worth" $ do
            let deck = deckFromList [Card Queen Spades, Card Three Clubs, Card King Hearts, Card Seven Hearts, Card Ace Spades, Card Ten Diamonds]
            deckScore deck `shouldBe` 15
            let deck' = deckFromList [Card Three Hearts, Card Queen Clubs, Card Four Hearts, Card Ace Diamonds, Card Ten Hearts]
            deckScore deck' `shouldBe` 3
            let deck'' = deckFromList [Card Three Clubs, Card Ace Spades, Card Ten Diamonds]
            deckScore deck'' `shouldBe` 0
            let deck''' = deckFromList [Card rank Hearts | rank <- [Two ..]]
            deckScore deck''' `shouldBe` 13

        context "any amount of Hearts less than all, with/without Queen of Spades in Deck" $ do
            prop "score is more than zero" $ do
                let allScoreCards = S.fromList $ Card Queen Spades : [Card rank Hearts | rank <- [Two ..]]
                    containsSomeScoreCards (Deck cards) = case S.toList onlyScoreCards of
                                                                [] -> False
                                                                _ -> onlyScoreCards `S.isProperSubsetOf` allScoreCards
                      where onlyScoreCards = cards `S.intersection` allScoreCards
                \deck -> if containsSomeScoreCards deck
                             then deckScore deck > 0
                             else otherwise

        context "when all Hearts and the Queen of Spades are in Deck" $ do
            it "a full deck sums up to zero" $ do
                deckScore makeDeck `shouldBe` 0
            prop "sums up to zero" $ do
                let allScoreCards = S.fromList $ Card Queen Spades : [Card rank Hearts | rank <- [Two ..]]
                \deck -> if allScoreCards == (allScoreCards `S.intersection` unDeck deck)
                             then deckScore deck == 0
                             else otherwise

    describe "emptyHand" $ do
        it "contains no cards" $ do
            handToList emptyHand `shouldBe` []

    describe "playHand" $ do
        prop "adds a card to the Hand" $ do
            let h = emptyHand
            \p c -> playHand (p, c) h == Hand (S.singleton (p, c))
        prop "does not add a card if player has already played" $ do
            let h = emptyHand
            \p c -> let h' = playHand (p, c) h in playHand (p, c) h' == h'
