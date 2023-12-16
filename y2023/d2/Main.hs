{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Control.Arrow ((>>>))
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.List (foldl')
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Text.Parsec

main :: IO ()
main = do
  putStrLn "AOC Day 2"
  input <- T.readFile "y2023/d2/task1.data"

  print "Task 1:"
  let resul1 = task1 input
  print resul1

  print "Task 2:"
  let result2 = task2 input
  print result2

task1 :: T.Text -> Int
task1 input =
  let games = parse (parseGame `sepBy1` newline) "task1 input" input & either (error . show) id
      validGames = games & filter (\(_, cubesSets) -> all (all isValidGame) cubesSets)
   in foldl'
        (\acc (gameId, _) -> acc + gameId)
        0
        validGames

data Bag = Bag
  { red :: Int,
    green :: Int,
    blue :: Int
  }

instance Show Bag where
  show (Bag r g b) = "(Red " ++ show r ++ " Green " ++ show g ++ " Blue " ++ show b ++ ")"

emptyBag :: Bag
emptyBag = Bag 0 0 0

referenceBag :: Bag
referenceBag = Bag {red = 12, green = 13, blue = 14}

minViableBag :: Bag -> Bag -> Bag
minViableBag (Bag red1 green1 blue1) (Bag red2 green2 blue2) =
  Bag
    { red = max red1 red2,
      green = max green1 green2,
      blue = max blue1 blue2
    }

bagPower :: Bag -> Int
bagPower (Bag r g b) = r * g * b

isValidGame :: Bag -> Bool
isValidGame (Bag r g b) =
  red referenceBag >= r
    && green referenceBag >= g
    && blue referenceBag >= b

colors :: [String]
colors = ["red", "green", "blue"]

parseGameId :: Parsec T.Text () Int
parseGameId = do
  string "Game"
  spaces
  num :: Int <- many1 digit <&> read
  string ":"
  pure num

parseCubes :: Parsec T.Text () Bag
parseCubes = do
  num :: Int <- many1 digit <&> read
  spaces
  color <- choice $ map (try . string) colors
  case color of
    "red" -> pure $ Bag {red = num, green = 0, blue = 0}
    "green" -> pure $ Bag {red = 0, green = num, blue = 0}
    "blue" -> pure $ Bag {red = 0, green = 0, blue = num}
    _ -> parserFail "unexpected color"

parseCubesSet :: Parsec T.Text () [Bag]
parseCubesSet = parseCubes `sepBy1` (char ',' >> spaces)

parseGame :: Parsec T.Text () (Int, [[Bag]])
parseGame = do
  gameId <- parseGameId
  spaces
  cubesSets <- parseCubesSet `sepBy1` (char ';' >> spaces)
  pure (gameId, cubesSets)

task2 :: T.Text -> Int
task2 input =
  let games = parse (parseGame `sepBy1` newline) "task1 input" input & either (error . show) id
      cubesSets = snd <$> games
      viableCubes :: [Bag] = cubesSets <&> (concat >>> foldl' minViableBag emptyBag)
   in viableCubes <&> bagPower & sum