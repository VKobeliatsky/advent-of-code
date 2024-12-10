{-# LANGUAGE LambdaCase #-}

module D4 where

import Data.Function
import Data.Functor
import Model
import Parser
import System.IO (readFile')

task1 = do
  print "Task1: "
  input <- readFile' "y2024/d4/input.data"
  let table = parseInput "input" input & either (error . show) id
  let result =
        ifoldl'
          ( \acc at c -> acc + countTargets allDirections at target table
          )
          0
          table
  print result

task2 = do
  print "Task2: "
  input <- readFile' "y2024/d4/input.data"
  let table = parseInput "input" input & either (error . show) id
  let result =
        ifoldl'
          ( \cases
              acc at c | c == 'A' -> case selectDiagonalNeighbors at table <&> snd of
                ['M', 'M', 'S', 'S'] -> acc + 1
                ['S', 'M', 'M', 'S'] -> acc + 1
                ['S', 'S', 'M', 'M'] -> acc + 1
                ['M', 'S', 'S', 'M'] -> acc + 1
                _ -> acc
              acc _ _ -> acc
          )
          0
          table
  print result

target = "XMAS"

countTargets ::
  [Direction] ->
  Coords ->
  String ->
  Table Char ->
  Int
countTargets
  directions
  at
  [last]
  table = case select at table of
    Just (_, c) | c == last -> 1
    _ -> 0
countTargets
  directions
  at
  target
  table = case select at table of
    Just (_, c)
      | c == head target ->
          sum (directions <&> \dir -> countTargets [dir] (nextCoord dir at) (tail target) table)
    _ -> 0
