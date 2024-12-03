module D3 where

import Data.Foldable
import Data.Function
import Data.Functor
import System.IO (readFile')
import Text.Parsec

task1 = do
  print "Task1: "
  input <- readFile' "y2024/d3/input.data"
  let muls = parse (many $ (try parseMul <&> uncurry (*)) <|> (anyChar $> 0)) "input" input & either (error . show) id
  let result = muls & foldl' (+) 0
  print result

type CharParser s u m t = (Stream s m Char) => ParsecT s u m t

parseInt :: CharParser s u m Int
parseInt = do
  ds <- many1 digit
  pure $ read ds

parseMul :: CharParser s u m (Int, Int)
parseMul = do
  string "mul("
  a <- parseInt
  char ','
  b <- parseInt
  char ')'
  pure (a, b)
