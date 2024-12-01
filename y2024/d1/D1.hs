module D1 where

import Data.Foldable (Foldable (foldl'))
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.Functor.Identity (Identity)
import Data.IntMap.Strict (IntMap, empty, findWithDefault, insertWith, toList)
import System.IO (readFile')
import Text.Parsec

task1 :: IO ()
task1 =
  do
    putStrLn "Task 1:"
    input <- readFile' "y2024/d1/input.data"
    let lists =
          either (error . show) id $
            runParser (parseAndMerge `sepBy` newline >> getState) ([], []) "input" input
    let result = addDistance 0 lists
    print result

task2 = do
  putStrLn "Task2:"
  input <- readFile' "y2024/d1/input.data"
  let (as, bs) =
        either (error . show) id $
          runParser
            (parseAndCount `sepBy` newline >> getState)
            (empty, empty)
            "input"
            input
  let result =
        foldl'
          (\acc (key, count) -> acc + key * count * findWithDefault 0 key bs)
          0
          $ toList as
  print result

type CharStream s = Stream s Identity Char

type Parser s u t = (CharStream s) => Parsec s u t

parseAndMerge :: (CharStream s) => Parsec s ([Int], [Int]) (Int, Int)
parseAndMerge = do
  (a, b) <- parsePair
  updateState (\(as, bs) -> (add a as, add b bs))
  pure (a, b)

parseAndCount :: (CharStream s) => Parsec s (IntMap Int, IntMap Int) (Int, Int)
parseAndCount = do
  (a, b) <- parsePair
  updateState $ countPair (a, b)
  pure (a, b)

parsePair :: Parser s u (Int, Int)
parsePair = do
  a <- parseInt
  spaces
  b <- parseInt
  pure (a, b)

parseInt :: Parser s u Int
parseInt = do
  ds <- many digit
  pure $ read ds

countPair :: (Int, Int) -> (IntMap Int, IntMap Int) -> (IntMap Int, IntMap Int)
countPair (a, b) (as, bs) =
  ( insertWith (+) a 1 as,
    insertWith (+) b 1 bs
  )

add n [] = [n]
add n [n1] =
  if n < n1
    then [n, n1]
    else [n1, n]
add n (n1 : rest) =
  if n < n1
    then n : n1 : rest
    else n1 : add n rest

addDistance d ([], bs) = d
addDistance d (as, []) = d
addDistance d (a : as, b : bs) = addDistance (d + abs (a - b)) (as, bs)
