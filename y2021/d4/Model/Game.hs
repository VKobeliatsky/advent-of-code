{-# LANGUAGE BangPatterns #-}

module Model.Game where

import Common
import Control.Arrow
import Control.Monad.ST
import Data.Foldable
import Data.Function
import Data.Functor
import Data.IntMap (IntMap)
import qualified Data.IntMap.Merge.Strict as IntMap
import qualified Data.IntMap.Strict as IntMap
import Data.STRef
import Data.Sequence ((|>))
import qualified Data.Sequence as Seq
import Model.Board
import Model.Cell

data Game s = Game
  { numbers :: ![Int],
    indexedCells :: !(CellsMap s)
  }

type SerializedGame = ([Int], SerializedCellsMap)

serializeGame :: Game s -> ST s SerializedGame
serializeGame game = do
  serializedBoards <- serializeCellsMap $ indexedCells game
  pure (numbers game, serializedBoards)

processNumber :: Game s -> ST s (Game s, [STRef s (BoardCell s Int)])
processNumber game@(Game [] _) = pure (game, [])
processNumber game@(Game (num : rest) indexedCells) =
  IntMap.lookup num indexedCells
    & maybe
      (pure (game {numbers = rest}, []))
      ( \cells -> do
          winners <- markAll cells
          pure (game {numbers = rest}, winners)
      )

playBingo :: Game s -> ST s (Maybe (BoardCell s Int))
playBingo (Game [] _) = pure Nothing
playBingo game = do
  (next, winners) <- processNumber game
  if null winners
    then playBingo next
    else Just <$> readSTRef (head winners)

type CellsMap s = IntMap [STRef s (BoardCell s Int)]

type SerializedCellsMap = IntMap [SerializedBoardCell Int]

mappedBoardFromLists :: CellsMap s -> [[Int]] -> ST s (CellsMap s)
mappedBoardFromLists cellsMap lists = do
  newBoardRef <- newSTRef undefined
  (cells, board) <-
    foldlM
      ( \(!cellsMap, !boardBuilder) (row, rowIx) ->
          foldlM
            ( \(!cellsMap, !rowBuilder) (x, colIx) -> do
                cell <- newSTRef $ newBoardCell x (rowIx, colIx) newBoardRef
                cells' <- addCell cellsMap cell
                pure (cells', rowBuilder |> cell)
            )
            (cellsMap, Seq.empty)
            (row `zip` [0 ..])
            <&> second (seqToVector >>> (boardBuilder |>))
      )
      (cellsMap, Seq.empty)
      (lists `zip` [0 ..])
      <&> second seqToVector
  writeSTRef newBoardRef board
  pure cells

serializeCellsMap :: CellsMap s -> ST s SerializedCellsMap
serializeCellsMap = mapM (mapM (\ref -> readSTRef ref <&> serializeCell))

emptyCellsMap :: IntMap a
emptyCellsMap = IntMap.empty

selectBoards :: CellsMap s -> ST s [STRef s (Board s Int)]
selectBoards =
  foldlM
    ( foldlM
        ( \boards cellRef -> do
            board <- readSTRef cellRef <&> boardRef
            pure $
              if board `notElem` boards
                then boards ++ [board]
                else boards
        )
    )
    []

cellsMapFromList :: [BoardCell s Int] -> ST s (CellsMap s)
cellsMapFromList =
  foldlM
    ( \cells cell -> do
        ref <- newSTRef cell
        addCell cells ref
    )
    emptyCellsMap

addCell :: CellsMap s -> STRef s (BoardCell s Int) -> ST s (CellsMap s)
addCell cellsMap cellRef = do
  cell <- readSTRef cellRef
  pure $ IntMap.insertWith (++) (boardCell cell & cellValue) [cellRef] cellsMap

markCell :: STRef s (BoardCell s Int) -> ST s (BoardCell s Int)
markCell = updateCell mark

isAWinnerCell :: BoardCell s Int -> ST s Bool
isAWinnerCell cell = do
  row <- selectRow cell >>= serializeSelection <&> fmap fst
  col <- selectCol cell >>= serializeSelection <&> fmap fst
  pure $ all isMarked row || all isMarked col

markAll :: [STRef s (BoardCell s Int)] -> ST s [STRef s (BoardCell s Int)]
markAll =
  foldlM
    ( \(!winners) ref -> do
        marked <- markCell ref
        isWinner <- isAWinnerCell marked
        pure $ if isWinner then winners |> ref else winners
    )
    Seq.empty
    >>> (<$>) toList

joinCellsMaps :: CellsMap s -> CellsMap s -> CellsMap s
joinCellsMaps =
  IntMap.merge
    IntMap.preserveMissing
    IntMap.preserveMissing
    (IntMap.zipWithMatched (const (++)))