module ModelSpec where

import Common
import Control.Monad.ST
import Data.Functor
import Model
import Test.Hspec

spec :: SpecWith ()
spec = describe "Model" $ do
  describe "STTable" $ do
    describe "stTableFromLists" $ do
      it "builds STTable from list of lists" $ do
        runST (stTableFromLists [[1, 2], [3, 4]] >>= freezeSTTable)
          `shouldBe` tableFromLists ([[1, 2], [3, 4]] <&> (<&> Empty))