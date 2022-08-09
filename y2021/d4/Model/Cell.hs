{-# LANGUAGE DeriveFunctor #-}

module Model.Cell where

data Cell a
  = Unmarked {cellValue :: !a}
  | Marked {cellValue :: !a}
  deriving (Show, Eq, Functor)

isMarked :: Cell a -> Bool
isMarked (Marked _) = True
isMarked _ = False

mark :: Cell a -> Cell a
mark (Unmarked val) = Marked val
mark cell = cell

type BingoCell = Cell Int

newCell :: a -> Cell a
newCell = Unmarked
