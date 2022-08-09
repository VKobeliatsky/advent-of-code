{-# LANGUAGE ScopedTypeVariables #-}

module ModelSpec where

import Common
import Control.Monad
import Control.Monad.ST
import Data.Function
import qualified Data.IntMap.Strict as IntMap
import Data.STRef.Strict
import Data.Vector ((!))
import Model.Board
import Model.Cell
import Model.Game
import Test.Hspec

spec :: SpecWith ()
spec = describe "Model" $ do
  describe "CellsMap" $ do
    describe "joinCellsMap" $ do
      it "preserves missing entries" $ do
        runST
          ( do
              newBoard <- newSTRef undefined
              c1 <- testCell newBoard 42 (69, 420)
              m1 <- cellsMapFromList [c1]
              c2 <- testCell newBoard 43 (70, 421)
              c3 <- testCell newBoard 42 (71, 422)
              m2 <- cellsMapFromList [c2, c3]
              let joined = joinCellsMaps m1 m2
              serializeCellsMap joined
          )
          `shouldBe` IntMap.fromList
            [ (42, [(Unmarked 42, (69, 420)), (Unmarked 42, (71, 422))]),
              (43, [(Unmarked 43, (70, 421))])
            ]

  describe "Board" $ do
    describe "selectRow" $ do
      it "selects a row" $ do
        runST
          ( do
              board <- testBoard [[1, 2], [3, 4]]
              cell <- readSTRef $ board ! 0 ! 0
              row <- selectRow cell
              mapM (readSTRef >=> pure . serializeCell) row
          )
          `shouldBe` [(Unmarked 1, (0, 0)), (Unmarked 2, (0, 1))]
    describe "selectCow" $ do
      it "selects a cow" $ do
        runST
          ( do
              board <- testBoard [[1, 2], [3, 4]]
              cell <- readSTRef $ board ! 1 ! 1
              row <- selectCol cell
              mapM (readSTRef >=> pure . serializeCell) row
          )
          `shouldBe` [(Unmarked 2, (0, 1)), (Unmarked 4, (1, 1))]

testCell :: STRef s (Board s a) -> a -> (Int, Int) -> ST s (BoardCell s a)
testCell boardRef a pos = pure $ BoardCell (newCell a) pos boardRef

testBoard :: [[Int]] -> ST s (Board s Int)
testBoard lists = do
  newBoard <- newSTRef undefined
  board <-
    tableIForM
      (tableFromLists lists)
      (\pos n -> newBoardCell n pos newBoard & newSTRef)
  writeSTRef newBoard board
  pure board