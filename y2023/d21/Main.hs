{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

module Main where

import           Common.RefMonad
import           Control.Monad
import           Control.Monad.Reader (MonadReader(..), ReaderT(runReaderT)
                                     , asks)
import           Data.Functor
import qualified Data.List as L
import qualified Data.Map.Strict as Map
import           Text.Parsec
import qualified System.IO
import           Control.Monad.IO.Class (MonadIO(..))
import           Data.IORef (IORef)
import           Control.Arrow
import           Prelude hiding (log)
import           Model
import           Parsers

targetSteps :: Int
targetSteps = 500

isTargetEven :: Bool
isTargetEven = even targetSteps

shouldMark :: (Integral a) => a -> Bool
shouldMark n = if isTargetEven
               then even n
               else odd n

main :: IO ()
main = do
  input <- readFile "y2023/d21/input.data"
  (template, startTiles) <- runParserT parseInputTable (0, 0) "" input
    <&> either (error . show) id
  tables <- newRef Map.empty
  starts <- newRef $ startTiles <&> ((0, 0), )
  state <- newRef 0 <&> TaskState (template, tables) starts
  runReaderT (mapM processStep [1 .. targetSteps]) (Env state System.IO.stdout)
  tilesCountView <- readRef (tilesCount state) <&> show
  putStrLn tilesCountView

type StackM s m = ( RefMonad m
                  , HasTaskState s (Ref m)
                  , HasLoggerHandle s
                  , MonadLogger m
                  , MonadReader s m)

class HasTaskState a ref where
  getTaskState :: a -> TaskState ref

class HasLoggerHandle a where
  getLoggerHandle :: a -> System.IO.Handle

class Monad m => MonadLogger m where
  log :: String -> m ()

data Env =
  Env { taskState :: TaskState IORef, loggerHandle :: System.IO.Handle }

instance HasTaskState Env IORef where
  getTaskState = taskState

instance HasLoggerHandle Env where
  getLoggerHandle = loggerHandle

instance (HasLoggerHandle env, MonadIO m) => MonadLogger (ReaderT env m) where
  log s = do
    handle <- asks getLoggerHandle
    liftIO
      $ do
        System.IO.hPutStr handle s
        System.IO.hFlush handle

update :: (StackM s m) => (s -> Ref m a) -> (a -> a) -> m a
update select updater = asks select >>= (`updateRef` updater)

incCount :: (StackM s m) => m Int
incCount = update (getTaskState >>> tilesCount) (+ 1)

processStep :: (MonadIO m, StackM s m) => Int -> m ()
processStep n =
  let marked = if isTargetEven
               then even n
               else odd n
      progressMessage = L.concat
        [ "Checking step: "
        , show n
        , "/"
        , show targetSteps
        , if n == targetSteps
          then "\n"
          else "\r"]
  in do
       log progressMessage
       asks (getTaskState >>> lastVisited)
         >>= readRef
         >>= mapM_
           (\coords -> do
              inputTable <- asks (getTaskState >>> table)
              getTileNeighborCoords coords inputTable
                `forM_` (\neighborCoords -> do
                           neighborRef <- getTile neighborCoords inputTable
                           tile <- readRef neighborRef
                           let isRock = isRockTile tile
                           let isVisited = isVisitedTile tile
                           when (not isRock && not isVisited)
                             $ do
                               when marked $ void incCount
                               updateRef neighborRef $ const $ Visited n marked
                               update (getTaskState >>> lastVisited)
                                 $ (:) neighborCoords
                               pure ()))
