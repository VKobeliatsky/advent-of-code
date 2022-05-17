{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}

module Main where

import Common
import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "updateAt" $ do
    describe "given an index within bounds" $ do
      it "yields Just and updates" $ do
        updateAt 2 (\case Just _ -> 42) [1, 2, 3, 4] `shouldBe` [1, 2, 42, 4]
    describe "given an index out of bounds" $ do
      it "yields Nothing and appends" $ do
        updateAt 42 (\case Nothing -> 4) [1, 2, 3] `shouldBe` [1, 2, 3, 4]