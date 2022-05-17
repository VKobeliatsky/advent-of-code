{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Parser where

import Common
import Control.Monad.ST
import Control.Monad.Trans
import Data.Functor
import Data.STRef
import Data.Sequence
import Model
import Text.Parsec

parseNumbers :: ParsecT String state (ST s) (STRef s (Seq Int))
parseNumbers = do
  ns <- many1 digit `sepBy` char ','
  newRef (fromList $ map read ns)

parseTable :: ParsecT String state (ST s) (STTable s Int)
parseTable = do
  lists <- parseLists
  lift (stTableFromLists lists)

parseLists ::
  Monad monad =>
  ParsecT String state monad [[Int]]
parseLists = parseRow `endBy1` eol

-- >>> parse parseLists "" "1 2 34\n5 6 78\n"
-- Right [[1,2,34],[5,6,78]]

parseRow ::
  Monad monad =>
  ParsecT String state monad [Int]
parseRow = (spacesOnly >> many1 digit) `sepBy1` spacesOnly <&> map read

-- >>> parse parseRow "" " 1 2 34"
-- Right [1,2,34]
