module D3 where

import Data.Foldable
import Data.Function
import Data.Functor
import System.IO (readFile')
import Text.Parsec

task1 = do
  print "Task1: "
  input <- readFile' "y2024/d3/input.data"
  let muls = parse parseMuls "input" input & either (error . show) id
  let result = muls & foldl' (+) 0
  print result

task2 = do
  print "Task: 2"
  input <- readFile' "y2024/d3/input.data"
  let parsed =
        runParser
          (parseCommands >> getState)
          (ParserState True 0)
          "input"
          input
          & either (error . show) id
  print $ result parsed

type CharParser s u m t = (Stream s m Char) => ParsecT s u m t

parseMuls :: CharParser s u m [Int]
parseMuls = many $ (try parseMul <&> uncurry (*)) <|> (anyChar $> 0)

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

data ParserState = ParserState
  { enabled :: Bool,
    result :: Int
  }
  deriving (Show, Eq)

parseDo :: CharParser s u m String
parseDo = string "do()"

parseDont :: CharParser s u m String
parseDont = string "don't()"

parseCommand :: CharParser s ParserState m ()
parseCommand =
  choice
    [ do
        try parseDo
        modifyState (\state -> state {enabled = True}),
      do
        try parseDont
        modifyState (\state -> state {enabled = False}),
      do
        (a, b) <- try parseMul
        modifyState (\state -> state {result = if enabled state then result state + a * b else result state})
    ]

parseCommands :: CharParser s ParserState m ()
parseCommands = do
  try parseCommand <|> (anyChar $> ())
  parseCommands <|> eof