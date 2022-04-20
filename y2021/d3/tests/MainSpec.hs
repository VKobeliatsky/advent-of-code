module Main where

import qualified Data.Vector as Vector
import Solutions
import Test.Hspec

testInput :: String
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
      describe "collectCounts" $ do
        it "should collect bit counts" $ do
          collectCounts "000\n111\n010\n" `shouldBe` (3, [1, 2, 1])
      describe "computePowerRates" $ do
        it "should yield correct rates" $ do
          computePowerRates testInput `shouldBe` (22, 9)
      describe "task 2" $ do
        describe "tableFromString" $ do
          it "should yield a correct table" $ do
            tableFromString "000\n111\n010\n"
              `shouldBe` Vector.fromList
                [ Vector.fromList "000",
                  Vector.fromList "111",
                  Vector.fromList "010"
                ]
        describe "findO2Rating" $ do
          it "should yield a correct row of bits" $ do
            findO2Rating (tableFromString "000\n111\n010\n")
              `shouldBe` Vector.fromList "010"
        describe "findCO2Rating" $ do
          it "should yield a correct row of bits" $ do
            findCO2Rating (tableFromString "000\n111\n010\n")
              `shouldBe` Vector.fromList "111"
        describe "computeScrubberRates" $ do
          it "should yield correct rates" $ do
            computeScrubberRates testInput `shouldBe` (23, 10)
