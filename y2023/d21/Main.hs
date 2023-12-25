{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

module Main where

import Common
import Common.RefMonad
import Control.Monad
import Control.Monad.Reader (MonadReader (..), ReaderT (runReaderT), asks)
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

  (inputTable, inputUnvisisted) <-
    runParserT parseInputTable (0, 0) "" input <&> either (error . show) id

  tilesCount <- newRef $ if isTargetEven then 1 else 0
  let state = TaskState inputTable inputUnvisisted tilesCount

  runReaderT
    (mapM processStep [1 .. targetSteps])
    state

  -- tableView <- serialize inputTable <&> tableRender renderTile
  tilesCountView <- readRef tilesCount <&> show

  putStrLn $
    L.intercalate
      "\n"
      [ "Task 1:",
        show targetSteps,
        -- tableView,
        tilesCountView
      ]

type StackM s m = (RefMonad m, MonadReader s m)

update :: (StackM s m) => (s -> Ref m a) -> (a -> a) -> m a
update select updater = asks select >>= (`updateRef` updater)

processStep :: (StackM (TaskState (Ref m)) m) => Int -> m ()
processStep n =
  let mark = shouldMark n
   in asks unvisisted
        >>= readRef
        >>= mapM_
          ( \ref -> do
              coords <- readRef ref <&> getCoords
              inputTable <- asks table
              selectNeighbors coords inputTable
                `forM_` ( \neighborRef -> do
                            tile <- readRef neighborRef
                            let neighborCoords = getCoords tile
                            let isRock = isRockTile tile
                            let isVisited = isVisitedTile tile
                            when (not isRock && not isVisited) $ do
                              when mark $ void incCount
                              updateRef neighborRef $ const $ Visited neighborCoords n mark
                              update unvisisted $ (:) neighborRef
                              pure ()
                        )
          )

type InputTable ref = Table (ref Tile)

type InfiniteInputTable m = Table (Ref m [(SectorCoords, Tile)])

type SectorCoords = (Int, Int)

data TaskState ref = TaskState
  { table :: InputTable ref,
    unvisisted :: ref [ref Tile],
    tilesCount :: ref Int
  }

data Tile
  = Garden {getCoords :: TableCoords}
  | Rock {getCoords :: TableCoords}
  | Visited {getCoords :: TableCoords, getScore :: Int, isResult :: Bool}
  deriving (Eq, Show)

incCount :: (StackM (TaskState (Ref m)) m) => m Int
incCount = update tilesCount (+ 1)

renderTile :: Tile -> String
renderTile (Visited _ n marked)
  | n == 0 = " S "
  | otherwise =
      let view = show n
       in L.concat $
            if marked
              then ["(", view, ")"]
              else [" ", view, " "]
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

parseInputTable :: (RefMonad m) => ParsecT String TableCoords m (InputTable (Ref m), Ref m [Ref m Tile])
parseInputTable = do
  setState (0, 0)
  rows <- parseRow `sepBy1` eol
  let inputMap = V.fromList (rows <&> fst)
  let starts = L.concatMap snd rows
  newRef starts <&> (inputMap,)

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