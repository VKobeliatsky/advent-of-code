{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}

module Common where

import Control.Arrow
import Control.Monad.ST
import Control.Monad.Trans
import Data.Foldable
import Data.Functor
import Data.IORef
import Data.STRef
import Data.Vector (Vector, (!))
import qualified Data.Vector as Vector
import System.Environment
import Text.Parsec

readGivenFile :: IO String
readGivenFile = do
  fileName <- getArgs <&> head
  readFile fileName

updateAt :: Int -> (Maybe a -> a) -> [a] -> [a]
updateAt n update xs = case splitAt n xs of
  (!as, []) -> as ++ [update Nothing]
  (!as, x : (!bs)) -> as ++ [update $ Just x] ++ bs

binToInt :: Foldable m => m Char -> Int
binToInt =
  fst
    . foldr'
      ( \case
          '0' -> \(int, power) -> (int + 0 * 2 ^ power, power + 1)
          '1' -> \(int, power) -> (int + 1 * 2 ^ power, power + 1)
          other -> error $ "unexpected char:" ++ [other]
      )
      (0, 0 :: Int)

eol :: Stream input monad Char => ParsecT input state monad Char
eol = char '\n'

spaceOnly :: Stream input monad Char => ParsecT input state monad Char
spaceOnly = char ' '

spacesOnly :: Stream input monad Char => ParsecT input state monad [Char]
spacesOnly = many spaceOnly

type Table a = Vector (Vector a)

tableFromLists :: [[a]] -> Table a
tableFromLists = map Vector.fromList >>> Vector.fromList

selectRow :: Int -> Table a -> Vector a
selectRow rowIx board = board ! rowIx

selectCol :: Int -> Table a -> [a]
selectCol colIx board =
  [ board ! rowIx ! colIx
    | rowIx <- [0 .. Vector.length board - 1]
  ]

class Monad monad => RefMonad monad ref | monad -> ref where
  newRef :: a -> monad (ref a)
  readRef :: ref a -> monad a
  writeRef :: ref a -> a -> monad ()
  updateRef :: ref a -> (a -> a) -> monad a
  updateRef ref f = do
    x <- readRef ref
    let result = f x
    writeRef ref result
    pure result

instance RefMonad IO IORef where
  newRef = newIORef
  readRef = readIORef
  writeRef = writeIORef

instance RefMonad (ST s) (STRef s) where
  newRef = newSTRef
  readRef = readSTRef
  writeRef = writeSTRef

instance RefMonad (ParsecT i u (ST s)) (STRef s) where
  newRef = lift . newRef
  readRef = lift . readRef
  writeRef ref val = lift $ writeRef ref val

parser :: ParsecT String (STRef s String) (ST s) (STRef s String)
parser = do
  str <- string "state"
  ref <- getState
  writeRef ref str
  return ref

-- >>> runST $ newSTRef "" >>= \ref -> runParserT parser ref "" "state" >>= readSTRef . either (const undefined) id
-- "state"
