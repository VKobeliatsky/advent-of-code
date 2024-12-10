module Model
  ( module AoC.Data.Table,
    Direction,
    allDirections,
    nextCoord,
    selectDiagonalNeighbors,
  )
where

import AoC.Data.Table
import Data.Function
import Data.Maybe
import Data.Vector (Vector, (!?))

data Direction = NW | N | NE | E | SE | S | SW | W
  deriving (Show, Eq)

allDirections = [NW, N, NE, E, SE, S, SW, W]

nextCoord :: Direction -> Coords -> Coords
nextCoord NW (i, j) = (i - 1, j - 1)
nextCoord N (i, j) = (i - 1, j)
nextCoord NE (i, j) = (i - 1, j + 1)
nextCoord E (i, j) = (i, j + 1)
nextCoord SE (i, j) = (i + 1, j + 1)
nextCoord S (i, j) = (i + 1, j)
nextCoord SW (i, j) = (i + 1, j - 1)
nextCoord W (i, j) = (i, j - 1)

selectDiagonalNeighbors ::
  Coords ->
  Table Char ->
  [(Coords, Char)]
selectDiagonalNeighbors
  coords
  table =
    [ select (nextCoord NW coords) table,
      select (nextCoord NE coords) table,
      select (nextCoord SE coords) table,
      select (nextCoord SW coords) table
    ]
      & catMaybes