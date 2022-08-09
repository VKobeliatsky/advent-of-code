{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Solutions where

import Common
import Control.Arrow
import Control.Monad.ST
import Data.Foldable
import Data.Function
import Data.Maybe
import Data.STRef
import Model.Board
import Model.Cell
import Model.Game
import Parser
import Text.Parsec

solution1 input = runST $ do
  result <- runParserT parseGame () "input" input
  maybeWinner <- either (error . show) playBingo result
  let winnerCell = fromMaybe (error "no winner") maybeWinner
  let winnerCellValue = boardCell >>> cellValue $ winnerCell
  winnerTable <- readSTRef (boardRef winnerCell) >>= serializeBoard
  let unmarkedCellsSum =
        winnerTable
          & ( selectElements
                >>> (fst <$>)
                >>> filter (not . isMarked)
                >>> (cellValue <$>)
                >>> foldl' (+) 0
            )
  pure (winnerCellValue, unmarkedCellsSum, winnerCellValue * unmarkedCellsSum)
