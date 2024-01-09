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

main :: IO ()
main = do
  input <- T.readFile "y2023/d4/input.data"
  let parsed = parse parseCards "input" input & either (error . show) id
  let result = foldl' (\acc (_, _, _, score) -> acc + score) 0 parsed
  print result

parseCards :: Parsec Text u [(Int, IntSet, [Int], Integer)]
parseCards = do
  parseCard `sepBy1` endOfLine

parseCard :: Parsec Text u (Int, IntSet, [Int], Integer)
parseCard = do
  string' "Card" >> spacesOnly
  cardId <- read <$> many1 digit
  char ':' >> spacesOnly
  winningNumbers <- IntSet.fromList
    <$> (read <$> many1 digit) `sepEndBy1` many1 spaceOnly
  char '|' >> spacesOnly
  numbers <- (many1 digit <&> read) `sepEndBy1` many1 spaceOnly
  let matchesCount = foldl'
        (\acc n -> if IntSet.member n winningNumbers
                   then acc + 1
                   else acc)
        0
        numbers
  let score = if matchesCount > 0
              then 2 ^ (matchesCount - 1)
              else 0
  pure (cardId, winningNumbers, numbers, score)