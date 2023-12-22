module Main where

import Common
import Control.Monad
import Control.Monad.State (StateT (..), execStateT, gets, modify')
import qualified Data.Bifunctor as BF
import Data.Foldable (foldlM)
import Data.Functor
import qualified Data.List as L
import qualified Data.Vector as V
import Text.Parsec

targetSteps :: Int
targetSteps = 64

isTargetEven :: Bool
isTargetEven = even targetSteps

shouldMark :: (Integral a) => a -> Bool
shouldMark n = if isTargetEven then even n else odd n

main :: IO ()
main = do
  input <- readFile "y2023/d21/input.data"

  (inputTable, startRefs) <- runParserT parseInputTable (0, 0) "" input <&> either (error . show) id

  result <-
    execStateT
      (mapM processStep [1 .. targetSteps])
      (TaskState inputTable startRefs (if isTargetEven then 1 else 0))

  tableView <- serialize (table result) <&> tableRender renderTile

  putStrLn $
    L.intercalate
      "\n"
      [ "Task 1:",
        show targetSteps,
        tableView,
        show $ tilesCount result
      ]

processStep :: (RefMonad m) => Int -> StateT (TaskState m) m ()
processStep n =
  let mark = shouldMark n
   in gets unvisisted
        >>= mapM_
          ( \ref -> do
              coords <- readRef ref <&> getCoords
              inputTable <- gets table
              selectNeighbors coords inputTable
                `forM_` ( \neighborRef -> do
                            tile <- readRef neighborRef
                            let neighborCoords = getCoords tile
                            let isRock = isRockTile tile
                            let isVisited = isVisitedTile tile
                            when (not isRock && not isVisited) $ do
                              when mark incCount
                              updateRef neighborRef $ const $ Visited neighborCoords n mark
                              modify' $ \s -> s {unvisisted = neighborRef : unvisisted s}
                              pure ()
                        )
          )

type InputTable m = Table (Ref m Tile)

data TaskState m = TaskState
  { table :: InputTable m,
    unvisisted :: [Ref m Tile],
    tilesCount :: Int
  }

data Tile
  = Garden {getCoords :: TableCoords}
  | Rock {getCoords :: TableCoords}
  | Visited {getCoords :: TableCoords, getScore :: Int, isResult :: Bool}
  deriving (Eq, Show)

incCount :: (Monad m) => StateT (TaskState m) m ()
incCount = modify' $ \s -> s {tilesCount = tilesCount s + 1}

renderTile :: Tile -> String
renderTile (Visited _ n b)
  | n == 0 = " S "
  | otherwise = L.concat $ if b then ["(", show n, ")"] else [" ", show n, " "]
renderTile (Garden _) = " . "
renderTile (Rock _) = " # "

isGardenTile :: Tile -> Bool
isGardenTile (Garden _) = True
isGardenTile _ = False

isRockTile :: Tile -> Bool
isRockTile (Rock _) = True
isRockTile _ = False

isVisitedTile :: Tile -> Bool
isVisitedTile (Visited {}) = True
isVisitedTile _ = False

mapVisitedTile :: a -> ((TableCoords, Int) -> a) -> Tile -> a
mapVisitedTile _ f (Visited coords n _) = f (coords, n)
mapVisitedTile def _ _ = def

mapVisitedTileM :: m a -> ((TableCoords, Int) -> m a) -> Tile -> m a
mapVisitedTileM _ f (Visited coords n _) = f (coords, n)
mapVisitedTileM def _ _ = def

parseInputTable :: (RefMonad m) => ParsecT String TableCoords m (InputTable m, [Ref m Tile])
parseInputTable = do
  setState (0, 0)
  rows <- parseRow `sepBy1` eol
  let inputMap = V.fromList (rows <&> fst)
  let starts = L.concatMap snd rows
  pure (inputMap, starts)

parseRow :: (RefMonad m) => ParsecT String TableCoords m (V.Vector (Ref m Tile), [Ref m Tile])
parseRow = do
  parsed <- many parseTile
  let tiles = V.fromList parsed
  starts <-
    foldlM
      ( \acc ref -> do
          tile <- readRef ref
          if isVisitedTile tile
            then pure (ref : acc)
            else pure acc
      )
      []
      parsed
  updateState (BF.bimap (+ 1) (const 0))
  pure (tiles, starts)

parseTile :: (RefMonad m) => ParsecT String TableCoords m (Ref m Tile)
parseTile = do
  coords <- getState
  tile <-
    char '.' $> Garden coords
      <|> char '#' $> Rock coords
      <|> char 'S' $> Visited coords 0 False
  updateState (BF.second (+ 1))
  newRef tile