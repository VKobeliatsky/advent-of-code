module ParserSpec where

import Common
import Control.Monad.ST
import Data.Either
import Data.Functor
import Data.STRef
import qualified Data.Sequence as Seq
import Model
import Parser
import Test.Hspec
import Text.Parsec

spec :: Spec
spec = describe "Parser" $ do
  describe "parseNumbers" $ do
    it "parses a sequence of numbers and yields a ref to it" $ do
      runST
        ( do
            result <- runParserT parseNumbers () "source" "1,2,34"
            readSTRef (fromRight (error "unexpected parse failure") result)
        )
        `shouldBe` Seq.fromList [1, 2, 34]
  describe "parseTable" $ do
    it "parses a STTable" $ do
      runST
        ( do
            table <- runParserT parseTable () "source" "1 2 3\n"
            freezeSTTable (fromRight (error "unexpected parse failure") table)
        )
        `shouldBe` tableFromLists ([[1, 2, 3]] <&> (<&> emptyCell))