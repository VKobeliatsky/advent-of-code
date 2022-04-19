{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Common
import Control.Arrow
import Data.Foldable
import Data.Maybe
import Data.Bifunctor
import Data.Function
import Data.Functor
import qualified Data.Vector as V


main :: IO ()
main = do
    input <- readGivenFile
    putStr "Task 1: "
    print (computePowerRates input & uncurry (*))
    putStr "Task 2: "
    print (computeScrubberRates input & uncurry (*))
    pure ()


computePowerRates :: String -> (Int, Int)
computePowerRates =
    collectCounts >>>
    collectRates >>>
    bimap binToInt binToInt

collectCounts :: String -> (Int, Int, [Int])
collectCounts = foldl'
            (\(!col, !row, !counts) -> \case
                '0' -> (col + 1, row, updateAt col (fromMaybe 0) counts)
                '1' -> (col + 1, row, updateAt col (maybe 1 (+1)) counts)
                '\n' -> (0, row + 1, counts)
                c -> error $ "unexpected input: " ++ [c]
            )
            (0, 1, [])

collectRates :: (Int, Int, [Int]) -> (String, String)
collectRates (_, rows, counts) = foldl'
    (\(!gamma, !epsilon) !count ->
    if count > rows `div` 2
        then (gamma ++ ['1'], epsilon ++ ['0'])
        else (gamma ++ ['0'], epsilon ++ ['1'])
    )
    ("", "")
    counts


computeScrubberRates :: String -> (Int, Int)
computeScrubberRates =
    collectRecords >>>
    recordsTable >>>
    (\table -> (table, table)) >>>
    bimap
        (findO2Rating >>> V.toList >>> binToInt)
        (findCO2Rating >>> V.toList >>> binToInt)

collectRecords :: String -> ([String], Int, Int)
collectRecords = foldl'
    (\(records, cols, rows) -> \case
        '\n' -> ([]:records, cols, rows + 1)
        c -> case records of
            (curr:rest) -> ((curr ++ [c]):rest, max (length curr + 1) cols, rows)
            rest -> ([c]:rest, max 1 cols, rows)
    )
    ([], 0, 0) . init

recordsTable :: ([String], Int, Int) -> Table Char
recordsTable (records, cols, rows) = records
    <&> padRight cols '0'
    <&> V.fromListN cols
    & V.fromListN rows

findO2Rating ::Table Char -> Row Char
findO2Rating = go 0 where
    go colIx table
        | V.length table == 1 = table V.! 0
        | otherwise = let
        (rows0, rows1) = V.partition (at colIx >>> (==) '0') table
        in go (colIx+1) $ if V.length rows0 > V.length rows1
            then rows0
            else rows1

findCO2Rating ::Table Char -> Row Char
findCO2Rating = go 0 where
    go colIx table
        | V.length table == 1 = table V.! 0
        | otherwise = let
        (rows0, rows1) = V.partition (at colIx >>> (==) '0') table
        in go (colIx+1) $ if V.length rows0 <= V.length rows1
            then rows0
            else rows1
