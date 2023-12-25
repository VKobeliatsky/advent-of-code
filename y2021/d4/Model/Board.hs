{-# LANGUAGE LambdaCase #-}

module Model.Board where

import           Common.RefMonad
import           Common.Table
import           Control.Arrow
import           Control.Monad
import           Control.Monad.ST
import           Data.Foldable
import           Data.Functor
import qualified Data.List as List
import           Data.STRef
import           Model.Cell
import           Text.Printf

type Board s a = Table (STRef s (BoardCell s a))

selectCol :: BoardCell s a -> ST s [STRef s (BoardCell s a)]
selectCol cell = do
  let (_, colIx) = boardCellPos cell
  table <- readRef $ boardRef cell
  pure $ tableCol colIx table

selectRow :: BoardCell s a -> ST s [STRef s (BoardCell s a)]
selectRow cell = do
  let (rowIx, _) = boardCellPos cell
  table <- readRef $ boardRef cell
  pure $ tableRow rowIx table

serializeSelection :: [STRef s (BoardCell s a)] -> ST s [SerializedBoardCell a]
serializeSelection refs = mapM readRef refs <&> (<$>) serializeCell

updateCell
  :: (Cell a -> Cell a) -> STRef s (BoardCell s a) -> ST s (BoardCell s a)
updateCell f targetRef =
  updateRef targetRef (\cell -> cell { boardCell = f $ boardCell cell })

type BingoBoard s = Board s Int

type SerializedBoard a = Table (SerializedBoardCell a)

printSerializedBoard :: SerializedBoard Int -> String
printSerializedBoard = fmap
  (fmap
     (fst
      >>> \case
        Marked x   -> printf "(%s)\t" (show x)
        Unmarked x -> printf "%s\t" (show x))
   >>> toList
   >>> join)
  >>> toList
  >>> List.intersperse "\n"
  >>> join

type SerializedBingoBoard = SerializedBoard Int

serializeBoard :: Board s a -> ST s (SerializedBoard a)
serializeBoard = tableMapM (\cell -> readSTRef cell <&> serializeCell)

data BoardCell s a = BoardCell { boardCell :: Cell a
                               , boardCellPos :: (Int, Int)
                               , boardRef :: STRef s (Board s a)
                               }

type BingoBoardCell s = BoardCell s Int

type SerializedBoardCell a = (Cell a, (Int, Int))

type SerializedBingoBoardCell = SerializedBoardCell Int

newBoardCell :: a -> (Int, Int) -> STRef s (Board s a) -> BoardCell s a
newBoardCell = BoardCell . newCell

serializeCell :: BoardCell s a -> SerializedBoardCell a
serializeCell (BoardCell val pos _) = (val, pos)

instance Show a => Show (BoardCell s a) where
  show x = "BoardCell " ++ show (boardCell x) ++ " " ++ show (boardCellPos x)
