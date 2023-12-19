{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Common (RefMonad (..))
import Control.Arrow
import Control.Monad
import Control.Monad.ST (runST)
import Control.Monad.State (StateT (..), evalStateT, get, gets, modify')
import Data.Bifunctor (bimap)
import Data.Char (digitToInt, isDigit)
import Data.Functor
import qualified Data.List as L
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Vector (Vector, (!?))
import qualified Data.Vector as V

main :: IO ()
main = do
  print "Task 1:"
  input <- T.readFile "y2023/d3/task1.data"
  print $ task1 input

task1 :: T.Text -> Int
task1 input = runST $ do
  ref <- newRef $ Number (0, False)
  evalStateT
    ( do
        mapM_ processChar (T.unpack input)
        state <- get
        markedElements <- mapM readRef (marked state)
        pure $ sum $ markedElements <&> mapNumber 0 fst
    )
    (TaskState V.empty (0, []) ref [])

type Schematic = Vector (Vector Char)

readSchematic :: T.Text -> Schematic
readSchematic input =
  let lists =
        T.foldl'
          (\(table, row) c -> if c == '\n' then (table ++ [row], []) else (table, row ++ [c]))
          ([], [])
          input
   in V.fromList $ V.fromList <$> fst lists

data TaskState m = TaskState
  { prevLine :: Vector (Ref m Element),
    currLine :: (Int, [Ref m Element]),
    currNumber :: Ref m Element,
    marked :: [Ref m Element]
  }

currIndex :: TaskState m -> Int
currIndex = currLine >>> fst

data Element
  = Number (Int, Bool)
  | Symbol
  | Dot
  deriving (Show, Eq)

mapNumber :: a -> ((Int, Bool) -> a) -> Element -> a
mapNumber _ f (Number x) = f x
mapNumber def _ _ = def

isNumber :: Element -> Bool
isNumber = mapNumber False (const True)

processChar :: (RefMonad m) => Char -> StateT (TaskState m) m ()
processChar currentChar = do
  when (isEol currentChar) finalizeLine

  when (isDot currentChar) $ do
    finalizeCurrentNumber
    newRef Dot >>= putToLine

  when (isDigit currentChar) $ do
    ref <- updateCurrentNumber currentChar
    hasAdjSymbols <- checkForSymbols
    when hasAdjSymbols $ mark ref
    putToLine ref

  when (isSymbol currentChar) $ do
    finalizeCurrentNumber
    markPrevLineNumbers
    prevNumber <- getPrevNumber
    mapM_ mark prevNumber
    newRef Symbol >>= putToLine
  where
    isEol = (==) '\n'
    isDot = (==) '.'
    isSymbol c = not (isDot c || isDigit c || isEol c)

    putToLine ref = modify' $ \s -> s {currLine = bimap (1 +) (ref :) (currLine s)}

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
      num <- gets currNumber >>= readRef
      unless (num == Number (0, False)) $ do
        newNumber <- newRef (Number (0, False))
        modify' (\s -> s {currNumber = newNumber})

    updateCurrentNumber c = do
      let nextDigit = digitToInt c
      ref <- gets currNumber
      updateRef ref (mapNumber (error "Number expected") $ first ((*) 10 >>> (+) nextDigit) >>> Number)
      pure ref

    checkForSymbols = do
      (idx, line) <- gets currLine
      row <- gets prevLine
      isLeftSymbol <-
        maybe (pure False) (fst >>> readRef >>> fmap (Symbol ==)) (L.uncons line)
      isLeftTopSymbol <-
        maybe (pure False) (fmap (Symbol ==) . readRef) (row !? (V.length row - idx - 2))
      isTopSymbol <-
        maybe (pure False) (fmap (Symbol ==) . readRef) (row !? (V.length row - idx - 1))
      isRightTopSymbol <-
        maybe (pure False) (fmap (Symbol ==) . readRef) (row !? (V.length row - idx))
      pure $ isLeftSymbol || isLeftTopSymbol || isTopSymbol || isRightTopSymbol

    getPrevNumber = do
      prev <- gets $ (currLine >>> snd >>> L.uncons) <&> (<&> fst)
      case prev of
        Nothing -> pure Nothing
        Just ref -> do
          element <- readRef ref
          pure $
            if isNumber element
              then Just ref
              else Nothing

    markPrevLineNumbers = do
      idx <- gets currIndex
      row <- gets prevLine

      maybe (pure ()) mark (row !? (V.length row - idx - 2))
      maybe (pure ()) mark (row !? (V.length row - idx - 1))
      maybe (pure ()) mark (row !? (V.length row - idx))
