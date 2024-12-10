module AoC.Data.Table where

import Data.Functor
import Data.Vector (Vector, (!?))
import Data.Vector qualified as Vector

type Table a = Vector (Vector a)

type Coords = (Int, Int)

select :: Coords -> Table a -> Maybe (Coords, a)
select (i, j) table = table !? i >>= (!? j) <&> ((i, j),)

ifoldl' :: (a -> (Int, Int) -> b -> a) -> a -> Table b -> a
ifoldl' f = Vector.ifoldl' (\acc rowIx -> Vector.ifoldl' (\acc colIx -> f acc (rowIx, colIx)) acc)