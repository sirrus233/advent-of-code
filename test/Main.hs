module Main (main) where

import Advent (SolutionId, readInput)
import Data.HashMap.Strict qualified as Map
import Solutions.All (solutions)
import Test.Hspec (Expectation, describe, hspec, it, shouldBe)

test :: SolutionId -> Integer -> Expectation
test solutionId expected = do
  case Map.lookup solutionId solutions of
    Nothing -> print $ "No solution found for " <> show @Text solutionId
    Just solution -> do
      input <- readInput solutionId
      solution input `shouldBe` Right expected

main :: IO ()
main = hspec $ do
  describe "Year 2015" $ do
    it "Day 01.1" $ do test (2015, 1, 1) 138
    it "Day 01.2" $ do test (2015, 1, 2) 1771
    it "Day 02.1" $ do test (2015, 2, 1) 1586300
    it "Day 02.2" $ do test (2015, 2, 2) 3737498
    it "Day 03.1" $ do test (2015, 3, 1) 2592
    it "Day 03.2" $ do test (2015, 3, 2) 2360
    it "Day 04.1" $ do test (2015, 4, 1) 282749
    it "Day 04.2" $ do test (2015, 4, 2) 9962624
    it "Day 05.1" $ do test (2015, 5, 1) 238
    it "Day 05.2" $ do test (2015, 5, 2) 69
    it "Day 06.1" $ do test (2015, 6, 1) 569999
    it "Day 06.2" $ do test (2015, 6, 2) 17836115
    it "Day 07.1" $ do test (2015, 7, 1) 3176
    it "Day 07.2" $ do test (2015, 7, 2) 14710
