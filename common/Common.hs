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
import Data.Sequence (Seq, (|>))
import qualified Data.Sequence as Seq
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import qualified Data.Vector.Mutable as MVector
import System.Environment
import Text.Parsec

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
  (!as, []) -> as ++ [update Nothing]
  (!as, x : (!bs)) -> as ++ [update $ Just x] ++ bs

binToInt :: (Foldable m) => m Char -> Int
binToInt =
  fst
    . foldr'
      ( \case
          '0' -> \(int, power) -> (int + 0 * 2 ^ power, power + 1)
          '1' -> \(int, power) -> (int + 1 * 2 ^ power, power + 1)
          other -> error $ "unexpected char:" ++ [other]
      )
      (0, 0 :: Int)

maybeToRight :: b -> Maybe a -> Either b a
maybeToRight b = maybe (Left b) Right

eol :: (Stream input monad Char) => ParsecT input state monad Char
eol = char '\n'

spaceOnly :: (Stream input monad Char) => ParsecT input state monad Char
spaceOnly = char ' '

spacesOnly :: (Stream input monad Char) => ParsecT input state monad [Char]
spacesOnly = many spaceOnly

type Table a = Vector (Vector a)

selectElements :: Table a -> [a]
selectElements = tableFoldl' (|>) Seq.empty >>> toList

tableFoldl' :: (b -> a -> b) -> b -> Table a -> b
tableFoldl' f = foldl' (foldl' f)

tableFoldr' :: (a -> b -> b) -> b -> Table a -> b
tableFoldr' f = foldr' (flip (foldr' f))

tableCol :: Int -> Table a -> [a]
tableCol colIx = foldl' (\col row -> col |> row Vector.! colIx) Seq.empty >>> toList

tableRow :: Int -> Table a -> [a]
tableRow rowIx = flip (Vector.!) rowIx >>> toList

tableMap :: (a -> b) -> Table a -> Table b
tableMap f = Vector.map (Vector.map f)

tableFor :: Table a -> (a -> b) -> Table b
tableFor = flip tableMap

tableIMap :: ((Int, Int) -> a -> b) -> Table a -> Table b
tableIMap f = Vector.imap (\rowIx -> Vector.imap (\colIx -> f (rowIx, colIx)))

tableIFor :: Table a -> ((Int, Int) -> a -> b) -> Table b
tableIFor = flip tableIMap

tableMapM :: (Monad m) => (a -> m b) -> Table a -> m (Table b)
tableMapM f = Vector.mapM (Vector.mapM f)

tableForM :: (Monad m) => Table a -> (a -> m b) -> m (Table b)
tableForM = flip tableMapM

tableIMapM :: (Monad m) => ((Int, Int) -> a -> m b) -> Table a -> m (Table b)
tableIMapM f = Vector.imapM (\rowIx -> Vector.imapM (\colIx -> f (rowIx, colIx)))

tableIForM :: (Monad m) => Table a -> ((Int, Int) -> a -> m b) -> m (Table b)
tableIForM = flip tableIMapM

tableMapM_ :: (Monad m) => (a -> m b) -> Table a -> m ()
tableMapM_ f = Vector.mapM_ (Vector.mapM_ f)

tableForM_ :: (Monad m) => Table a -> (a -> m b) -> m ()
tableForM_ = flip tableMapM_

tableIMapM_ :: (Monad m) => ((Int, Int) -> a -> m b) -> Table a -> m ()
tableIMapM_ f = Vector.imapM_ (\rowIx -> Vector.imapM_ (\colIx -> f (rowIx, colIx)))

tableIForM_ :: (Monad m) => Table a -> ((Int, Int) -> a -> m b) -> m ()
tableIForM_ = flip tableIMapM_

tableFromLists :: [[a]] -> Table a
tableFromLists = map Vector.fromList >>> Vector.fromList

class (Monad monad) => RefMonad monad ref | monad -> ref where
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

seqToVector :: Seq a -> Vector a
seqToVector as =
  Vector.create
    ( do
        v <- MVector.new $ Seq.length as
        Seq.traverseWithIndex (MVector.write v) as
        pure v
    )
