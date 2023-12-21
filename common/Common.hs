{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}

module Common where

import Control.Arrow
import Control.Monad.ST
import Control.Monad.State (StateT)
import Control.Monad.Trans (lift)
import Data.Foldable
import Data.Functor
import Data.IORef
import Data.Kind
import Data.Maybe (maybeToList)
import Data.STRef
import Data.Sequence (Seq, (|>))
import qualified Data.Sequence as Seq
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Vector (Vector, (!?))
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

seqToVector :: Seq a -> Vector a
seqToVector as =
  Vector.create
    ( do
        v <- MVector.new $ Seq.length as
        Seq.traverseWithIndex (MVector.write v) as
        pure v
    )

type Table a = Vector (Vector a)

tableRender :: (a -> String) -> Table a -> String
tableRender renderTile =
  Vector.foldl'
    ( \tableView row ->
        tableView
          ++ Vector.foldl'
            (\rowView a -> rowView ++ renderTile a ++ " ")
            ""
            row
          ++ "\n"
    )
    ""

type RefTable m a = Vector (Vector (Ref m a))

serialize :: (RefMonad m) => RefTable m a -> m (Table a)
serialize = tableMapM readRef

type TableCoords = (Int, Int)

selectElements :: Table a -> [a]
selectElements = tableFoldl' (|>) Seq.empty >>> toList

selectNeighbors :: (Int, Int) -> Table a -> [a]
selectNeighbors (i, j) table = do
  let t = maybeToList (table !? (i - 1) >>= (!? j))
  let r = maybeToList (table !? i >>= (!? (j + 1)))
  let b = maybeToList (table !? (i + 1) >>= (!? j))
  let l = maybeToList (table !? i >>= (!? (j - 1)))
  t ++ r ++ b ++ l

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

class (Monad monad) => RefMonad monad where
  type Ref monad :: Type -> Type
  newRef :: a -> monad (Ref monad a)
  readRef :: Ref monad a -> monad a
  writeRef :: Ref monad a -> a -> monad ()
  updateRef :: Ref monad a -> (a -> a) -> monad a
  updateRef ref f = do
    x <- readRef ref
    let result = f x
    writeRef ref result
    pure result

instance RefMonad IO where
  type Ref IO = IORef
  newRef = newIORef
  readRef = readIORef
  writeRef = writeIORef

instance RefMonad (ST s) where
  type Ref (ST s) = STRef s
  newRef = newSTRef
  readRef = readSTRef
  writeRef = writeSTRef

instance (RefMonad m) => RefMonad (StateT s m) where
  type Ref (StateT s m) = Ref m
  newRef = lift . newRef
  readRef = lift . readRef
  writeRef ref val = lift $ writeRef ref val

instance (RefMonad m) => RefMonad (ParsecT i u m) where
  type Ref (ParsecT i u m) = Ref m
  newRef = lift . newRef
  readRef = lift . readRef
  writeRef ref val = lift $ writeRef ref val
