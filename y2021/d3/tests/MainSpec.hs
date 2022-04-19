module Main where

import Solutions
import Test.Hspec

testInput =
  unlines
    [ "00100",
      "11110",
      "10110",
      "10111",
      "10101",
      "01111",
      "00111",
      "11100",
      "10000",
      "11001",
      "00010",
      "01010"
    ]

main :: IO ()
main = hspec $ do
  describe "y2021/d3" $ do
    describe "task 1" $ do
      describe "computePowerRates" $ do
        it "should yield correct rates" $ do
          computePowerRates testInput `shouldBe` (22, 9)
