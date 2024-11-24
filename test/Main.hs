module Main (main) where

import Advent (AnySolution (..), SolutionId, readInput)
import Data.HashMap.Strict qualified as Map
import Solutions.All (solutions)
import Test.Hspec (Expectation, describe, hspec, it, shouldBe)

test :: SolutionId -> Integer -> Expectation
test solutionId expected = do
  case Map.lookup solutionId solutions of
    Nothing -> print $ "No solution found for " <> show @Text solutionId
    Just (AnySolution solution) -> do
      input <- readInput solutionId
      (fromIntegral <$> solution input) `shouldBe` Right expected

main :: IO ()
main = hspec $ do
  describe "Year 2015" $ do
    it "Day 01.1" $ do test (2015, 01, 1) 138
    it "Day 01.2" $ do test (2015, 01, 2) 1771
    it "Day 02.1" $ do test (2015, 02, 1) 1586300
    it "Day 02.2" $ do test (2015, 02, 2) 3737498
    it "Day 03.1" $ do test (2015, 03, 1) 2592
    it "Day 03.2" $ do test (2015, 03, 2) 2360
    it "Day 04.1" $ do test (2015, 04, 1) 282749
    it "Day 04.2" $ do test (2015, 04, 2) 9962624
    it "Day 05.1" $ do test (2015, 05, 1) 238
    it "Day 05.2" $ do test (2015, 05, 2) 69
    it "Day 06.1" $ do test (2015, 06, 1) 569999
    it "Day 06.2" $ do test (2015, 06, 2) 17836115
    it "Day 07.1" $ do test (2015, 07, 1) 3176
    it "Day 07.2" $ do test (2015, 07, 2) 14710
    it "Day 08.1" $ do test (2015, 08, 1) 1350
    it "Day 08.2" $ do test (2015, 08, 2) 2085
    it "Day 09.1" $ do test (2015, 09, 1) 207
    it "Day 09.2" $ do test (2015, 09, 2) 804
    it "Day 10.1" $ do test (2015, 10, 1) (-1)
    it "Day 10.2" $ do test (2015, 10, 2) (-1)
  describe "Year 2019" $ do
    it "Day 01.1" $ do test (2019, 01, 1) 3184233
    it "Day 01.2" $ do test (2019, 01, 2) 4773483
    it "Day 02.1" $ do test (2019, 02, 1) 3409710
    it "Day 02.2" $ do test (2019, 02, 2) 7912
