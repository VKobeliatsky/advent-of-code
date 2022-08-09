module ParserSpec where

import Control.Monad.ST
import Data.Either
import Parser
import Test.Hspec
import Text.Parsec

spec :: Spec
spec = describe "Parser" $ do
  describe "parseNumbers" $ do
    it "parses a list of numbers and yields a ref to it" $ do
      runST
        ( do
            result <- runParserT parseNumbers () "source" "1,2,34"
            pure $ fromRight (error "unexpected parse failure") result
        )
        `shouldBe` [1, 2, 34]