{-# LANGUAGE MultiWayIf #-}

module Model where

import           Common.Table
import           Data.Map.Strict (Map)
import           Common.RefMonad
import qualified Data.Map.Strict as Map
import qualified Data.List as L
import           Data.Vector ((!?), (!))
import qualified Data.Vector as V
import           Data.Functor
import qualified Data.Bifunctor as BF
import           Data.Maybe (fromMaybe)

type InputTable ref = Table (ref Tile)

type InfiniteInputTable ref =
  (Table Tile, ref (Map SectorCoords (ref (InputTable ref))))

type SectorCoords = (Int, Int)

data TaskState ref =
  TaskState { table :: InfiniteInputTable ref
            , lastVisited :: ref [(SectorCoords, TableCoords)]
            , tilesCount :: ref Int
            }

data Tile = Garden
          | Rock
          | Visited { getScore :: Int, isMarked :: Bool }
  deriving (Eq, Show)

renderTile :: Tile -> String
renderTile (Visited n marked)
  | n == 0 = " S "
  | otherwise = let view = "O"
                in L.concat
                   $ if marked
                     then ["(", view, ")"]
                     else [" ", view, " "]
renderTile Garden = " . "
renderTile Rock = " # "

isGardenTile :: Tile -> Bool
isGardenTile Garden = True
isGardenTile _ = False

isRockTile :: Tile -> Bool
isRockTile Rock = True
isRockTile _ = False

isVisitedTile :: Tile -> Bool
isVisitedTile (Visited {}) = True
isVisitedTile _ = False

getTable :: RefMonad m
         => SectorCoords
         -> InfiniteInputTable (Ref m)
         -> m (InputTable (Ref m))
getTable sector (template, tablesRef) = do
  tables <- readRef tablesRef
  maybe
    (do
       newTable <- template `tableForM` newRef
       newTableRef <- newRef newTable
       updateRef tablesRef (Map.insert sector newTableRef)
       pure newTable)
    readRef
    (Map.lookup sector tables)

getTile :: RefMonad m
        => (SectorCoords, TableCoords)
        -> InfiniteInputTable (Ref m)
        -> m (Ref m Tile)
getTile (sector, (i, j)) infiniteTable = do
  table <- getTable sector infiniteTable
  pure
    $ fromMaybe (error $ "Expected Just Tile at: " ++ show (sector, (i, j)))
    $ table !? i >>= (!? j)

getTileNeighborCoords :: (SectorCoords, TableCoords)
                      -> InfiniteInputTable ref
                      -> [(SectorCoords, (Int, Int))]
getTileNeighborCoords (sector, coords) (template, _) =
  let rowCount = V.length template
      lastRowIdx = rowCount - 1
      colCount = V.length (template ! 0)
      lastColIdx = colCount - 1
  in getNeighborCoords coords
     <&> \(i, j) -> if
       | i == -1 -> (BF.first (\i -> i - 1) sector, (lastRowIdx, j))
       | i == rowCount -> (BF.first (+ 1) sector, (0, j))
       | j == -1 -> (BF.second (\j -> j - 1) sector, (i, lastColIdx))
       | j == colCount -> (BF.second (+ 1) sector, (i, 0))
       | otherwise -> (sector, (i, j))
