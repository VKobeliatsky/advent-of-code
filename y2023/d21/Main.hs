{-# LANGUAGE LambdaCase #-}

module Main where

import Common
import Control.Monad.ST (runST)
import Data.Bifunctor (Bifunctor (bimap, second))
import Data.Foldable (foldlM)
import Data.Functor
import qualified Data.List as L
import qualified Data.Vector as V
import Debug.Trace (trace)
import Text.Parsec

main :: IO ()
main = do
  input <- readFile "y2023/d21/input.data"
  let steps = 16
  result <- runParserT parseInputMap (0, 0) "" input
  let (inputMap, starts) = either (error . show) id result
  -- debug
  serializedStarts <- mapM readRef starts
  trace ("inputMap:\n\t" ++ show inputMap) $ pure ()
  trace ("starts:\n\t" ++ show serializedStarts) $ pure ()
  -- debug
  print "Hello Day 21"

type InputMap = Table Tile

data Tile
  = Garden (Int, Int)
  | Rock (Int, Int)
  | Start (Int, Int)
  deriving (Eq, Show)

isStart :: Tile -> Bool
isStart (Start _) = True
isStart _ = False

mapStartTile :: a -> ((Int, Int) -> a) -> Tile -> a
mapStartTile _ f (Start coords) = f coords
mapStartTile def _ _ = def

mapStartTileM :: m a -> ((Int, Int) -> m a) -> Tile -> m a
mapStartTileM _ f (Start coords) = f coords
mapStartTileM def _ _ = def

parseInputMap :: (RefMonad m) => ParsecT String (Int, Int) m (InputMap, [Ref m Tile])
parseInputMap = do
  setState (0, 0)
  rows <- parseRow `sepBy1` eol
  let inputMap = V.fromList (rows <&> fst)
  let starts = L.concatMap snd rows
  pure (inputMap, starts)

parseRow :: (RefMonad m) => ParsecT String (Int, Int) m (V.Vector Tile, [Ref m Tile])
parseRow = do
  parsed <- many parseTile
  let tiles = V.fromList parsed
  starts <-
    foldlM
      ( curry $ \case
          (acc, start@(Start _)) -> do
            ref <- newRef start
            pure (ref : acc)
          (acc, _) -> pure acc
      )
      []
      parsed
  updateState (bimap (+ 1) (const 0))
  pure (tiles, starts)

parseTile :: (Monad m) => ParsecT String (Int, Int) m Tile
parseTile = do
  coords <- getState
  tile <-
    char '.' $> Garden coords
      <|> char '#' $> Rock coords
      <|> char 'S' $> Start coords
  updateState (second (+ 1))
  pure tile