{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import qualified Common

import Data.Foldable
import Control.Arrow
import Data.Functor

main :: IO ()
main = do
    input :: [Int] <- Common.readGivenFile <&> (lines >>> map read)
    print "task 1: "
    print $ show $ increases input
    print "task 2: "
    print $ show $ slidingIncreases input

increases :: [Int] -> Int
increases [] = 0
increases (depth:rest) = snd $ foldl'
    (\(!prevDepth, !count) currDepth ->
        if currDepth > prevDepth
            then (currDepth, count + 1)
            else (currDepth, count)
    )
    (depth, 0)
    rest

-- >>>increases [199, 200, 208, 210, 200, 207, 240, 269, 260, 263]
-- 7

slidingIncreases :: [Int] -> Int
slidingIncreases (n1:ns@(n2:n3:_)) = let
    go (a:rest@(b:c:_)) (!prevWindow, !count) = let
        nextWindow = a + b + c
        in 
            go rest (nextWindow, if nextWindow > prevWindow then count + 1 else count)
    go _ a = a
    in
        snd $ go ns (n1 + n2 + n3, 0)
slidingIncreases _ = 0
-- >>> slidingIncreases [199, 200, 208, 210, 200, 207, 240, 269, 260, 263]
