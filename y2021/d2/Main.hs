{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import qualified Common
import Data.Foldable (Foldable(foldl'))
import Data.Functor ( (<&>) )


data Instruction
    = Forward !Int
    | Up !Int
    | Down !Int
    deriving (Show)

readInstruction :: String -> Instruction
readInstruction input = case words input of
    ["forward", distance] -> Forward $ read distance
    ["up", distance] -> Up $ read distance
    ["down", distance] -> Down $ read distance
    _ -> error $ "unrecognized Instruction: " ++ input


data Position = Position
    { horizontal :: !Int
    , depth :: !Int
    , aim :: !Int
    }
    deriving (Show)


main :: IO ()
main = do
    instructions <- Common.readGivenFile <&> map readInstruction . lines
    putStrLn "Task 1:"
    let destination = endPosition instructions
    print (horizontal destination * depth destination)
    putStrLn "Task 2:"
    let destination' = endPosition' instructions
    print (horizontal destination' * depth destination')


endPosition :: [Instruction] -> Position
endPosition = foldl'
    (\position instruction -> case instruction of
        Forward distance -> position {horizontal = horizontal position + distance}
        Up distance -> position {depth = depth position - distance}
        Down distance -> position {depth = depth position + distance}
    )
    (Position 0 0 0)

endPosition' :: [Instruction] -> Position
endPosition' = foldl'
    (\position instruction -> case instruction of
        Forward distance -> position {
            horizontal = horizontal position + distance,
            depth = depth position + distance * aim position
        }
        Up distance -> position { aim = aim position - distance }
        Down distance -> position { aim = aim position + distance }
    )
    (Position 0 0 0)