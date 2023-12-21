{-# LANGUAGE TupleSections #-}

module Main where

import Common
import Control.Arrow
import Control.Monad
import qualified Data.Bifunctor as BF
import Data.Foldable (foldlM)
import Data.Functor
import qualified Data.List as L
import qualified Data.Vector as V
import Text.Parsec

main :: IO ()
main = do
  input <- readFile "y2023/d21/input.data"
  (inputTable, starts) <- runParserT parseInputTable (0, 0) "" input <&> either (error . show) id

  result <- go (0, inputTable, starts)
  print $ "Task1: " ++ show result
  where
    targetSteps = 64
    go (step, _, refs) | step == targetSteps = pure $ length refs
    go state = processStep state >>= go

processStep :: (RefMonad m) => (Int, InputTable m, [Ref m Tile]) -> m (Int, InputTable m, [Ref m Tile])
processStep (prevStep, table, starts) = do
  let step = prevStep + 1
  nextStarts <-
    foldlM
      ( \acc ref -> do
          (_, coords) <- readRef ref <&> mapStartTile (error "Start expected") id

          refs <-
            filterM
              ( \ref -> do
                  tile <- readRef ref
                  let isRock = isRockTile tile
                  let isAccounted = mapStartTile False (fst >>> (==) step) tile
                  pure (not isRock && not isAccounted)
              )
              (selectNeighbors coords table)

          mapM_ (`updateRef` (Start . (step,) . getTileCoords)) refs
          updateRef ref (Garden . getTileCoords)

          pure $ acc ++ refs
      )
      []
      starts

  pure (step, table, nextStarts)

type InputTable m = Table (Ref m Tile)

data Tile
  = Garden TableCoords
  | Rock TableCoords
  | Start (Int, TableCoords)
  deriving (Eq)

instance Show Tile where
  show (Garden coords) = ". " ++ show coords
  show (Rock coords) = "# " ++ show coords
  show (Start coords) = "S " ++ show coords

isGardenTile :: Tile -> Bool
isGardenTile (Garden _) = True
isGardenTile _ = False

isRockTile :: Tile -> Bool
isRockTile (Rock _) = True
isRockTile _ = False

isStartTile :: Tile -> Bool
isStartTile (Start _) = True
isStartTile _ = False

getTileCoords :: Tile -> TableCoords
getTileCoords (Start (_, coords)) = coords
getTileCoords (Garden coords) = coords
getTileCoords (Rock coords) = coords

mapStartTile :: a -> ((Int, TableCoords) -> a) -> Tile -> a
mapStartTile _ f (Start coords) = f coords
mapStartTile def _ _ = def

mapStartTileM :: m a -> ((Int, TableCoords) -> m a) -> Tile -> m a
mapStartTileM _ f (Start coords) = f coords
mapStartTileM def _ _ = def

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
          if isStartTile tile
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
      <|> char 'S' $> Start (0, coords)
  updateState (BF.second (+ 1))
  newRef tile