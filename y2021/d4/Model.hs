{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Model where

import Common
import Control.Monad.ST
import Data.Function
import Data.Functor
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import qualified Data.Vector as Vector
import Data.Vector.Mutable
import qualified Data.Vector.Mutable as MVector

data Cell a
  = Empty {value :: a}
  | Checked {value :: a}
  deriving (Show, Eq, Functor)

emptyCell :: a -> Cell a
emptyCell = Empty

type CellsMap s = IntMap (STCell s Int)

addCell :: STCell s Int -> CellsMap s -> CellsMap s
addCell stCell = IntMap.insert (cell stCell & value) stCell

type STTable s a = STVector s (STVector s (STCell s a))

data STCell s a = STCell
  { cell :: Cell a,
    collPos :: (Int, Int),
    boardRef :: STTable s a
  }

instance Show a => Show (STCell s a) where
  show stCell =
    "STCell" <> show (cell stCell) <> show (collPos stCell)

updateSTCell :: (Cell a -> Cell a) -> STCell s a -> STCell s a
updateSTCell f stCell = stCell {cell = f $ cell stCell}

stTableFromLists :: [[a]] -> ST s (STTable s a)
stTableFromLists lists = do
  let table = Vector.fromList $ lists <&> Vector.fromList
  stTable <- MVector.new $ Vector.length table
  Vector.imapM_
    ( \rowIx vector ->
        let row =
              Vector.imap
                (\colIx a -> STCell (emptyCell a) (colIx, rowIx) stTable)
                vector
         in do
              stRow <- Vector.thaw row
              MVector.write stTable rowIx stRow
    )
    table
  pure stTable

freezeSTTable :: STTable s a -> ST s (Table (Cell a))
freezeSTTable stTable = do
  x <- Vector.freeze stTable
  y <- mapM Vector.freeze x
  pure (y <&> (cell <$>))

modifySTTable :: (STCell s a -> STCell s a) -> (Int, Int) -> STTable s a -> ST s ()
modifySTTable f (rowIx, colIx) stTable = do
  row <- MVector.read stTable rowIx
  MVector.modify row f colIx

readSTTable :: (Int, Int) -> STTable s a -> ST s (STCell s a)
readSTTable (rowIx, colIx) stTable = MVector.read stTable rowIx >>= flip MVector.read colIx
