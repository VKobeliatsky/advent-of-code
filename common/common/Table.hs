module Common.Table where

import           Common.RefMonad
import           Data.Vector (Vector, (!?))
import qualified Data.Vector as Vector
import           Data.Sequence ((|>))
import qualified Data.Sequence as Seq
import           Control.Arrow
import           Data.Foldable
import           Data.Maybe

type Table a = Vector (Vector a)

tableRender :: (a -> String) -> Table a -> String
tableRender renderTile = Vector.foldl'
  (\tableView row -> tableView
   ++ Vector.foldl' (\rowView a -> rowView ++ renderTile a ++ " ") "" row
   ++ "\n")
  ""

type RefTable m a = Vector (Vector (Ref m a))

serialize :: (RefMonad m) => RefTable m a -> m (Table a)
serialize = tableMapM readRef

type TableCoords = (Int, Int)

selectElements :: Table a -> [a]
selectElements = tableFoldl' (|>) Seq.empty >>> toList

selectNeighbors :: (Int, Int) -> Table a -> [a]
selectNeighbors (i, j) table = do
  let t = maybeToList (table !? (i - 1) >>= (!? j))
  let r = maybeToList (table !? i >>= (!? (j + 1)))
  let b = maybeToList (table !? (i + 1) >>= (!? j))
  let l = maybeToList (table !? i >>= (!? (j - 1)))
  t ++ r ++ b ++ l

tableFoldl' :: (b -> a -> b) -> b -> Table a -> b
tableFoldl' f = foldl' (foldl' f)

tableFoldr' :: (a -> b -> b) -> b -> Table a -> b
tableFoldr' f = foldr' (flip (foldr' f))

tableCol :: Int -> Table a -> [a]
tableCol colIx = foldl' (\col row -> col |> row Vector.! colIx) Seq.empty
  >>> toList

tableRow :: Int -> Table a -> [a]
tableRow rowIx = flip (Vector.!) rowIx >>> toList

tableMap :: (a -> b) -> Table a -> Table b
tableMap f = Vector.map (Vector.map f)

tableFor :: Table a -> (a -> b) -> Table b
tableFor = flip tableMap

tableIMap :: ((Int, Int) -> a -> b) -> Table a -> Table b
tableIMap f = Vector.imap (\rowIx -> Vector.imap (\colIx -> f (rowIx, colIx)))

tableIFor :: Table a -> ((Int, Int) -> a -> b) -> Table b
tableIFor = flip tableIMap

tableMapM :: (Monad m) => (a -> m b) -> Table a -> m (Table b)
tableMapM f = Vector.mapM (Vector.mapM f)

tableForM :: (Monad m) => Table a -> (a -> m b) -> m (Table b)
tableForM = flip tableMapM

tableIMapM :: (Monad m) => ((Int, Int) -> a -> m b) -> Table a -> m (Table b)
tableIMapM f = Vector.imapM
  (\rowIx -> Vector.imapM (\colIx -> f (rowIx, colIx)))

tableIForM :: (Monad m) => Table a -> ((Int, Int) -> a -> m b) -> m (Table b)
tableIForM = flip tableIMapM

tableMapM_ :: (Monad m) => (a -> m b) -> Table a -> m ()
tableMapM_ f = Vector.mapM_ (Vector.mapM_ f)

tableForM_ :: (Monad m) => Table a -> (a -> m b) -> m ()
tableForM_ = flip tableMapM_

tableIMapM_ :: (Monad m) => ((Int, Int) -> a -> m b) -> Table a -> m ()
tableIMapM_ f = Vector.imapM_
  (\rowIx -> Vector.imapM_ (\colIx -> f (rowIx, colIx)))

tableIForM_ :: (Monad m) => Table a -> ((Int, Int) -> a -> m b) -> m ()
tableIForM_ = flip tableIMapM_

tableFromLists :: [[a]] -> Table a
tableFromLists = map Vector.fromList >>> Vector.fromList

