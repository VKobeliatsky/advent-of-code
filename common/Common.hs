{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}

module Common where

import           Data.Foldable
import           Data.Functor
import           Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import qualified Data.Text as T
import qualified Data.Text.IO as T
import           Data.Vector (Vector)
import qualified Data.Vector as Vector
import qualified Data.Vector.Mutable as MVector
import           System.Environment
import           Text.Parsec

readGivenFile :: IO String
readGivenFile = do
  fileName <- getArgs <&> head
  readFile fileName

readGivenFileText :: IO T.Text
readGivenFileText = do
  fileName <- getArgs <&> head
  T.readFile fileName

updateAt :: Int -> (Maybe a -> a) -> [a] -> [a]
updateAt n update xs = case splitAt n xs of
  (!as, [])      -> as ++ [update Nothing]
  (!as, x:(!bs)) -> as ++ [update $ Just x] ++ bs

binToInt :: (Foldable m) => m Char -> Int
binToInt = fst
  . foldr'
    (\case
       '0'   -> \(int, power) -> (int + 0 * 2 ^ power, power + 1)
       '1'   -> \(int, power) -> (int + 1 * 2 ^ power, power + 1)
       other -> error $ "unexpected char:" ++ [other])
    (0, 0 :: Int)

maybeToRight :: b -> Maybe a -> Either b a
maybeToRight b = maybe (Left b) Right

eol :: (Stream input monad Char) => ParsecT input state monad Char
eol = char '\n'

spaceOnly :: (Stream input monad Char) => ParsecT input state monad Char
spaceOnly = char ' '

spacesOnly :: (Stream input monad Char) => ParsecT input state monad [Char]
spacesOnly = many spaceOnly

seqToVector :: Seq a -> Vector a
seqToVector as = Vector.create
  (do
     v <- MVector.new $ Seq.length as
     Seq.traverseWithIndex (MVector.write v) as
     pure v)
