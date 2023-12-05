{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Common
import Data.Function
import Data.Functor
import Data.List (foldl', singleton)
import Text.Parsec

main :: IO ()
main = do
  input <- readGivenFileText
  let parsed =
        either (error . show) id $
          parse
            ( sepEndBy
                ( do
                    skipMany letter
                    (digit <&> read . singleton) `sepEndBy` many letter
                )
                endOfLine
            )
            "input.data"
            input
  let correction = parsed <&> (\row -> head row * 10 + last row) & foldl' (+) 0
  print correction
