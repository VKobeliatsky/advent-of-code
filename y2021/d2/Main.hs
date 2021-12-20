{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
module Main where

import qualified Common
import Control.Arrow ( (>>>) )
import Data.Foldable (Foldable(foldl'))
import Data.Functor ( (<&>) )


data Instruction
    = Forward !Int
    | Up !Int
    | Down !Int
    deriving (Show)

readInstruction :: String -> Instruction
readInstruction = words >>> \case
    ["forward", distance] -> Forward $ read distance
    ["up", distance] -> Up $ read distance
    ["down", distance] -> Down $ read distance
    rest -> error $ "unrecognized Instruction: " ++ show rest


data Position = Position
    { horizontal :: !Int
    , depth :: !Int
    }
    deriving (Show)


main :: IO ()
main = do
    instructions <- Common.readGivenFile <&> map readInstruction . lines
    putStrLn "Task 1:"
    let destination = endPosition instructions
    print (horizontal destination * depth destination)


endPosition :: [Instruction] -> Position
endPosition = foldl'
    (\position instruction -> case instruction of
        Forward distance -> position {horizontal = horizontal position + distance}
        Up distance -> position {depth = depth position - distance}
        Down distance -> position {depth = depth position + distance}
    )
    (Position 0 0)
