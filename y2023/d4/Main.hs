{-# LANGUAGE Strict #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Data.Text (Text)
import qualified Data.Text.IO as T
import           Text.Parsec
import           Data.Functor
import           Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet
import           Common
import           Data.Function
import           Data.Foldable
import           Data.IntMap (IntMap, (!?))
import qualified Data.IntMap.Strict as IntMap
import           Control.Arrow
import           Data.Maybe (fromMaybe)
import           Control.Monad

main :: IO ()
main = do
  input <- T.readFile "y2023/d4/input.data"
  task1 input
  task2 input

task1 :: Text -> IO ()
task1 input = do
  let parsed = parse parseCards "input" input & either (error . show) id
  let result = foldl'
        (\acc (_, _, _, score) -> if score > 0
                                  then acc + 2 ^ (score - 1)
                                  else acc)
        0
        parsed
  print result

task2 :: Text -> IO ()
task2 input = do
  let (_, (_, total)) =
        runParser parseCardsAndCollectCopies (IntMap.empty, 0) "input" input
        & either (error . show) id
  print total

parseCardsAndCollectCopies :: Parsec
                             Text
                             (IntMap Int, Int)
                             ([(Int, IntSet, [Int], Int)], (IntMap Int, Int))
parseCardsAndCollectCopies = do
  cards <- sepBy1
    (do
       card@(cardId, _, _, score) <- parseCard
       (copiesCounts, total) <- getState
       let currentCount = (copiesCounts !? cardId & fromMaybe 0) + 1
       let withCurrentOriginal = IntMap.insert cardId currentCount copiesCounts
       let cardsToUpdate = take score [cardId + 1 ..]
       let updatedCounts = foldl'
             (\acc key -> IntMap.insertWith (+) key currentCount acc)
             withCurrentOriginal
             cardsToUpdate
       setState (updatedCounts, total + currentCount)
       pure card)
    endOfLine
  result <- getState
  pure (cards, result)

parseCards :: Parsec Text u [(Int, IntSet, [Int], Int)]
parseCards = do
  parseCard `sepBy1` endOfLine

parseCard :: Parsec Text u (Int, IntSet, [Int], Int)
parseCard = do
  string' "Card" >> spacesOnly
  cardId <- read <$> many1 digit
  char ':' >> spacesOnly
  winningNumbers <- IntSet.fromList
    <$> (read <$> many1 digit) `sepEndBy1` many1 spaceOnly
  char '|' >> spacesOnly
  numbers <- (many1 digit <&> read) `sepEndBy1` many1 spaceOnly
  let score = foldl'
        (\acc n -> if IntSet.member n winningNumbers
                   then acc + 1
                   else acc)
        0
        numbers
  pure (cardId, winningNumbers, numbers, score)