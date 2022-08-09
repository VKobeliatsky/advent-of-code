{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Parser where

import Common
import Control.Monad.ST
import Control.Monad.Trans
import Data.Functor
import Model.Game
import Text.Parsec

parseGame :: ParsecT String state (ST s) (Game s)
parseGame = do
  ns <- parseNumbers <* eol
  eol
  cellsMap <- parseMappedBoard `sepEndBy1` eol <&> foldl1 joinCellsMaps
  pure $ Game ns cellsMap

parseNumbers :: Monad monad => ParsecT String state monad [Int]
parseNumbers = do
  ns <- many1 digit `sepBy` char ','
  pure $ map read ns

parseMappedBoard :: ParsecT String state (ST s) (CellsMap s)
parseMappedBoard = parseLists >>= lift . mappedBoardFromLists emptyCellsMap

parseLists :: Monad monad => ParsecT String state monad [[Int]]
parseLists = parseRow `sepEndBy1` eol

-- >>> parse parseLists "" "1 2 34\n5 6 78"
-- Right [[1,2,34],[5,6,78]]

parseRow :: Monad monad => ParsecT String state monad [Int]
parseRow = (spacesOnly >> many1 digit) `sepBy1` spacesOnly <&> map read

-- >>> parse parseRow "" " 1 2 34"
-- Right [1,2,34]
