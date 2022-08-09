{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Solutions where

import Common
import Control.Arrow
import Data.Bifunctor
import Data.Foldable hiding (toList)
import Data.Function
import Data.Maybe
import Data.Vector hiding (filter, foldl', init, length, map, (++))
import qualified Data.Vector as Vector

type Row a = Vector a

at :: Int -> Vector a -> a
at = flip (!)

task1 :: String -> Int
task1 = computePowerRates >>> uncurry (*)

task2 :: String -> Int
task2 = computeScrubberRates >>> uncurry (*)

computePowerRates :: String -> (Int, Int)
computePowerRates =
  collectCounts
    >>> collectRates
    >>> bimap binToInt binToInt

collectCounts :: String -> (Int, [Int])
collectCounts =
  foldl'
    ( \(!col, !row, !counts) -> \case
        '0' -> (col + 1, row, updateAt col (fromMaybe 0) counts)
        '1' -> (col + 1, row, updateAt col (maybe 1 (+ 1)) counts)
        '\n' -> (0, row + 1, counts)
        c -> error $ "unexpected input: " ++ [c]
    )
    (0, 0, [])
    >>> \(_, rows, counts) -> (rows, counts)

collectRates :: (Int, [Int]) -> (String, String)
collectRates (rows, counts) =
  foldl'
    ( \(!gamma, !epsilon) !count ->
        if count > rows `div` 2
          then (gamma ++ ['1'], epsilon ++ ['0'])
          else (gamma ++ ['0'], epsilon ++ ['1'])
    )
    ("", "")
    counts

computeScrubberRates :: String -> (Int, Int)
computeScrubberRates =
  tableFromString
    >>> ( \table ->
            ( table & findO2Rating & binToInt,
              table & findCO2Rating & binToInt
            )
        )

tableFromString :: String -> Table Char
tableFromString = lines >>> map fromList >>> fromList

findO2Rating :: Table Char -> Row Char
findO2Rating = go 0
  where
    go colIx table
      | Vector.length table == 1 = table ! 0
      | otherwise =
        let (rows0, rows1) = partition (at colIx >>> (==) '0') table
         in go (colIx + 1) $
              if Vector.length rows0 > Vector.length rows1
                then rows0
                else rows1

findCO2Rating :: Table Char -> Row Char
findCO2Rating = go 0
  where
    go colIx rows
      | Vector.length rows == 0 = Vector.empty
      | Vector.length rows == 1 = rows ! 0
      | otherwise =
        let (rows0, rows1) = partition (at colIx >>> (==) '0') rows
         in go (colIx + 1) $
              if Vector.length rows0 <= Vector.length rows1
                then rows0
                else rows1
