{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BangPatterns #-}
module Common where

import System.Environment
import Data.Functor
import qualified Data.Vector as V
import Data.Foldable

readGivenFile :: IO String
readGivenFile = do
    fileName <- getArgs <&> head
    readFile fileName


safeAt :: [a] -> Int -> Maybe a
safeAt [] _ = Nothing
safeAt (a:as) n
    | n < 0 = Nothing
    | n == 0 = Just a
    | otherwise = safeAt as (n-1)


updateAt :: Int -> (Maybe a -> a) -> [a] -> [a]
updateAt n update xs = case splitAt n xs of
    (!as, []) -> as ++ [update Nothing]
    (!as, x : (!bs)) -> as ++ [update $ Just x] ++ bs


safeHead :: [a] -> Maybe a
safeHead as = safeAt as 0


padRight :: Int -> a -> [a] -> [a]
padRight targetLength a as =
    let currentLength = length as
    in if targetLength >= currentLength
        then as ++ replicate (currentLength - targetLength) a
        else as


binToInt :: String -> Int
binToInt = fst . foldr'
    (\case
        '0' -> \(int, power) -> (int + 0 * 2^power, power + 1)
        '1' -> \(int, power) -> (int + 1 * 2^power, power + 1)
        other -> error $ "unexpected char:" ++ [other]
    )
    (0, 0 :: Int)


type Row a = V.Vector a
type Table a = V.Vector (V.Vector a)

at :: Int -> V.Vector a -> a
at = flip (V.!)