module Common where

import System.Environment
import Data.Functor

readGivenFile :: IO String
readGivenFile = do
    fileName <- getArgs <&> head
    readFile fileName