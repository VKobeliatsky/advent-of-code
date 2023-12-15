{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Control.Arrow
import Data.Foldable (find)
import Data.Functor
import Data.List (foldl', singleton)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Text.Parsec

main :: IO ()
main = do
  task1
  task2

task1 :: IO ()
task1 = do
  print "Task 1:"
  input <- T.readFile "y2023/d1/task1.data"
  let parsed =
        either (error . show) id $
          parse
            ( sepEndBy
                ( do
                    skipMany letter
                    (digit <&> read . singleton) `sepEndBy` many letter
                )
                endOfLine
            )
            "input.data"
            input
  print $ correction parsed

correction :: [[Int]] -> Int
correction = foldl' (+) 0 . fmap (\row -> head row * 10 + last row)

digits :: [String]
digits =
  [ "1",
    "2",
    "3",
    "4",
    "5",
    "6",
    "7",
    "8",
    "9",
    "0",
    "one",
    "two",
    "three",
    "four",
    "five",
    "six",
    "seven",
    "eight",
    "nine",
    "zero"
  ]

digitToNumber :: T.Text -> Int
digitToNumber "one" = 1
digitToNumber "two" = 2
digitToNumber "three" = 3
digitToNumber "four" = 4
digitToNumber "five" = 5
digitToNumber "six" = 6
digitToNumber "seven" = 7
digitToNumber "eight" = 8
digitToNumber "nine" = 9
digitToNumber "zero" = 0
digitToNumber "1" = 1
digitToNumber "2" = 2
digitToNumber "3" = 3
digitToNumber "4" = 4
digitToNumber "5" = 5
digitToNumber "6" = 6
digitToNumber "7" = 7
digitToNumber "8" = 8
digitToNumber "9" = 9
digitToNumber "0" = 0
digitToNumber other = error $ "unexpected digit:" ++ T.unpack other

task2 :: IO ()
task2 = do
  print "Task 2:"
  input <- T.readFile "y2023/d1/task2.data"
  let parsed :: [[Int]] = parseTable input
  print $ correction parsed

parseLine :: T.Text -> [Int]
parseLine =
  foldl'
    ( \currentRow restOfLine ->
        let record =
              find
                (fst >>> flip T.isPrefixOf restOfLine)
                digitsTable
         in maybe currentRow (\(_, number) -> currentRow ++ [number]) record
    )
    []
    . T.tails

parseTable :: T.Text -> [[Int]]
parseTable = T.lines >>> fmap parseLine

spelledDigit :: [Parsec T.Text () Int]
spelledDigit =
  digitsTable <&> \(spelled, number) -> do
    let (firstLetter, rest) = T.splitAt 1 spelled
    string' (T.unpack firstLetter)
    lookAhead $ string' (T.unpack rest)
    pure number

digitsTable :: [(T.Text, Int)]
digitsTable =
  [ ("1", 1),
    ("2", 2),
    ("3", 3),
    ("4", 4),
    ("5", 5),
    ("6", 6),
    ("7", 7),
    ("8", 8),
    ("9", 9),
    ("0", 0),
    ("one", 1),
    ("two", 2),
    ("three", 3),
    ("four", 4),
    ("five", 5),
    ("six", 6),
    ("seven", 7),
    ("eight", 8),
    ("nine", 9),
    ("zero", 0)
  ]