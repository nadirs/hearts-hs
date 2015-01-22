module HeartsSpec (spec) where

-- base modules
import Prelude hiding (foldr)
import Data.List (sort, group)
import qualified Data.Set as S
import Data.Foldable (foldr)
-- testing modules
import Test.Hspec
import Test.Hspec.QuickCheck
-- our modules
import Game.Hearts

{-# ANN module "HLint: ignore Redundant do" #-}

spec :: Spec
spec = do
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
            (keepDuplicates . group . sort . toList) deck `shouldBe` []

        context "as a Foldable instance" $
            it "can be folded to its card count" $
                foldr (const succ) 0 makeDeck `shouldBe` (52 :: Int)


    describe "deckScore" $ do
        it "counts how many points a deck is worth" $ do
            let deck = fromList [Card Queen Spades, Card Three Clubs, Card King Hearts, Card Seven Hearts, Card Ace Spades, Card Ten Diamonds]
            deckScore deck `shouldBe` 15
            let deck' = fromList [Card Three Hearts, Card Queen Clubs, Card Four Hearts, Card Ace Diamonds, Card Ten Hearts]
            deckScore deck' `shouldBe` 3
            let deck'' = fromList [Card Three Clubs, Card Ace Spades, Card Ten Diamonds]
            deckScore deck'' `shouldBe` 0
            let deck''' = fromList [Card rank Hearts | rank <- [Two ..]]
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
