module Parsers where

import           Common.RefMonad
import           Text.Parsec
import           Common.Table
import           Model
import qualified Data.Vector as V
import           Data.Functor
import           Common
import qualified Data.List as L
import           Data.Foldable
import qualified Data.Bifunctor as BF

parseInputTable
  :: (RefMonad m) => ParsecT String TableCoords m (Table Tile, [TableCoords])
parseInputTable = do
  setState (0, 0)
  rows <- parseRow `sepBy1` eol
  let inputMap = V.fromList (rows <&> fst)
  let starts = L.concatMap snd rows
  pure (inputMap, starts)

parseRow :: (RefMonad m)
         => ParsecT String TableCoords m (V.Vector Tile, [TableCoords])
parseRow = do
  parsed <- many parseTile
  let tiles = V.fromList parsed <&> \(tile, _, _) -> tile
  let starts = foldl'
        (\acc (_, coords, isStart) -> if isStart
                                      then coords:acc
                                      else acc)
        []
        parsed
  updateState (BF.bimap (+ 1) (const 0))
  pure (tiles, starts)

parseTile
  :: (RefMonad m) => ParsecT String TableCoords m (Tile, TableCoords, Bool)
parseTile = do
  coords <- getState
  (tile, isStart) <- char '.' $> (Garden, False)
    <|> char '#' $> (Rock, False)
    <|> char 'S' $> (Garden, True)
  updateState (BF.second (+ 1))
  pure (tile, coords, isStart)