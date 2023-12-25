{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StrictData #-}

module Main where

import Common.RefMonad
import Control.Arrow hiding (second)
import Control.Monad
import Control.Monad.ST (runST)
import Control.Monad.State (StateT (..), evalStateT, get, gets, modify')
import Data.Bifunctor (bimap, second)
import Data.Char (digitToInt, isDigit)
import Data.Function ((&))
import Data.Functor
import qualified Data.List as L
import Data.Maybe (maybeToList)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Vector (Vector, (!?))
import qualified Data.Vector as V

main :: IO ()
main = do
  print "Task 1:"
  input <- T.readFile "y2023/d3/task1.data"
  print $ task1 input

task1 :: T.Text -> (Int, Int)
task1 input =
  runST $
    evalStateT
      ( do
          mapM_ processChar (T.unpack input)
          state <- get
          markedElements <- mapM readRef (marked state)
          gearPowers <-
            mapM readRef (gears state)
              >>= (fmap snd >>> mapM (mapM readRef))
              <&> fmap
                ( \case
                    [Number (a, _), Number (b, _)] -> a * b
                    _ -> 0
                )
          let result1 = sum $ markedElements <&> mapNumber 0 fst
          let result2 = sum gearPowers
          pure (result1, result2)
      )
      ( TaskState
          { prevLine = V.empty,
            currLine = (0, []),
            currNumber = Nothing,
            gears = [],
            marked = []
          }
      )

data TaskState m = TaskState
  { prevLine :: Vector (Ref m Element),
    currLine :: (Int, [Ref m Element]),
    currNumber :: Maybe (Ref m Element),
    gears :: [Ref m (Ref m Element, [Ref m Element])],
    marked :: [Ref m Element]
  }

currIndex :: TaskState m -> Int
currIndex = currLine >>> fst

data Element
  = Number (Int, Bool)
  | Gear
  | Symbol
  | Dot
  deriving (Show, Eq)

mapNumber :: a -> ((Int, Bool) -> a) -> Element -> a
mapNumber _ f (Number x) = f x
mapNumber def _ _ = def

isNumberElem :: Element -> Bool
isNumberElem = mapNumber False (const True)

isGearElem :: Element -> Bool
isGearElem Gear = True
isGearElem _ = False

isSymbolElem :: Element -> Bool
isSymbolElem Symbol = True
isSymbolElem a = isGearElem a

processChar ::
  ( RefMonad m,
    Eq (Ref m Element)
  ) =>
  Char ->
  StateT (TaskState m) m ()
processChar currentChar = do
  when (isEol currentChar) $ do
    finalizeCurrentNumber
    finalizeLine

  when (isDot currentChar) $ do
    finalizeCurrentNumber
    newRef Dot >>= putToLine

  when (isDigit currentChar) $ do
    ref <- updateCurrentNumber currentChar
    hasAdjSymbols <- checkForSymbols
    when hasAdjSymbols $ mark ref
    neighborGears <- getNeighborElements >>= filterM (readRef >>> fmap isGearElem)
    records <-
      gets gears
        >>= filterM
          ( readRef
              >>> fmap
                ( do
                    gearRef <- fst
                    numberRefs <- snd
                    let isNeighbor = gearRef `L.elem` neighborGears
                    let hasCurrentNumber = ref `L.elem` numberRefs
                    pure $ isNeighbor && not hasCurrentNumber
                )
          )
    forM_ records (`updateRef` second (ref :))
    putToLine ref

  when (isSymbol currentChar) $ do
    prevNumber <- finalizeCurrentNumber
    markPrevLineNumbers
    maybe
      (pure ())
      mark
      prevNumber

    when (isGear currentChar) $ do
      refs <- getNeighborElements >>= filterM (readRef >>> fmap isNumberElem) <&> L.nub
      gear <- newRef Gear
      gearRecord <- newRef (gear, refs)
      modify' $ \s -> s {gears = gearRecord : gears s}
      putToLine gear

    unless (isGear currentChar) $
      newRef Symbol >>= putToLine
  where
    isEol = (==) '\n'
    isDot = (==) '.'
    isGear = (==) '*'
    isSymbol c = not (isEol c || isDot c || isDigit c)

    putToLine ref = modify' $ \s -> s {currLine = bimap (1 +) (ref :) (currLine s)}

    getNeighborElements =
      do
        idx <- gets currIndex
        row <- gets prevLine
        prev <- gets (currLine >>> snd >>> L.uncons >>> fmap fst)

        pure $
          [ row !? (V.length row - idx - 2),
            row !? (V.length row - idx - 1),
            row !? (V.length row - idx),
            prev
          ]
            <&> maybeToList
            & L.foldl1' (++)

    mark ref = do
      element <- readRef ref
      mapNumber
        (pure ())
        ( \(x, wasMarked) -> do
            writeRef ref (Number (x, True))
            unless wasMarked $
              modify' (\s -> s {marked = ref : marked s})
        )
        element

    finalizeLine = do
      (_, line) <- gets currLine
      let vector = V.fromList line
      modify' (\s -> s {prevLine = vector, currLine = (0, [])})

    finalizeCurrentNumber = do
      number <- gets currNumber
      maybe
        (pure ())
        (const $ modify' (\s -> s {currNumber = Nothing}))
        number
      pure number

    updateCurrentNumber c = do
      let nextDigit = digitToInt c
      maybeRef <- gets currNumber
      ref <-
        maybe
          ( do
              newNumber <- newRef $ Number (0, False)
              modify' (\s -> s {currNumber = Just newNumber})
              pure newNumber
          )
          pure
          maybeRef
      updateRef ref (mapNumber (error "Number expected") $ first ((*) 10 >>> (+) nextDigit) >>> Number)
      pure ref

    checkForSymbols = do
      (idx, line) <- gets currLine
      row <- gets prevLine
      isLeftSymbol <-
        maybe (pure False) (fst >>> readRef >>> fmap isSymbolElem) (L.uncons line)
      isLeftTopSymbol <-
        maybe (pure False) (fmap isSymbolElem . readRef) (row !? (V.length row - idx - 2))
      isTopSymbol <-
        maybe (pure False) (fmap isSymbolElem . readRef) (row !? (V.length row - idx - 1))
      isRightTopSymbol <-
        maybe (pure False) (fmap isSymbolElem . readRef) (row !? (V.length row - idx))
      pure $ isLeftSymbol || isLeftTopSymbol || isTopSymbol || isRightTopSymbol

    markPrevLineNumbers = do
      idx <- gets currIndex
      row <- gets prevLine

      maybe (pure ()) mark (row !? (V.length row - idx - 2))
      maybe (pure ()) mark (row !? (V.length row - idx - 1))
      maybe (pure ()) mark (row !? (V.length row - idx))
