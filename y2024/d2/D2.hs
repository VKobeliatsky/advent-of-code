module D2 where

import Control.Monad (when)
import Data.Bool (bool)
import Data.Foldable
import Data.Function
import System.IO (readFile')
import Text.Parsec

task1 = do
  input <- readFile' "y2024/d2/input.data"
  let result =
        parse
          ((parseInt `sepBy` char ' ') `sepBy` newline)
          "input"
          input
  let reports = either (error . show) id result

  let result = reports & foldl' (\count report -> if isReportSafe report then count + 1 else count) 0
  print result

withinBounds n = n >= 1 && n <= 3

isReportSafe :: [Int] -> Bool
isReportSafe (a : b : rest) =
  let (result, _, _) =
        foldl'
          ( \acc@(isSafe, direction, prev) next ->
              if not isSafe
                then acc
                else
                  let diff = prev - next
                      isSafe = (signum diff == direction) && withinBounds (abs diff)
                   in (isSafe, direction, next)
          )
          (withinBounds $ abs $ a - b, signum (a - b), b)
          rest
   in result

parseInt = do
  nums <- many1 digit
  pure $ read nums
