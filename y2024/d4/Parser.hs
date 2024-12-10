module Parser (parseInput) where

import AoC.Data.Table (Table)
import Data.Vector (Vector, (!))
import Data.Vector qualified as Vector
import Text.Parsec

type CharParser s u m t = (Stream s m Char) => ParsecT s u m t

parseInput :: SourceName -> String -> Either ParseError (Table Char)
parseInput = parse $ parseLines ([], 0) >>= \(rows, n) -> pure $ Vector.fromListN n rows

parseLines (ls, n) =
  choice
    [ parseLine >>= next,
      eof >> done
    ]
  where
    next line = parseLines (ls ++ [line], n + 1)
    done = pure (ls, n)

parseLine = do
  (s, count) <- parseChars ([], 0)
  pure $ Vector.fromListN count s

parseChars (s, n) =
  choice
    [ char 'X' >>= next,
      char 'M' >>= next,
      char 'A' >>= next,
      char 'S' >>= next,
      newline >> done
    ]
  where
    next c = parseChars (s ++ [c], n + 1)
    done = pure (s, n)
