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
