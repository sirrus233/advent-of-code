module Main (main) where

import Advent (AnySolution (..), Solution, SolutionId, readInput)
import Data.HashMap.Strict qualified as Map
import Data.Typeable (cast)
import Solutions.All (solutions)
import Test.Hspec (Expectation, describe, expectationFailure, hspec, it, parallel, shouldBe)

test :: (Typeable a, Eq a, Show a) => SolutionId -> a -> Expectation
test solutionId expected = do
  case Map.lookup solutionId solutions of
    Nothing -> print $ "No solution found for " <> show @Text solutionId
    Just (AnySolution solution) -> case cast solution of
      Just (solution' :: Solution a) -> do
        input <- readInput solutionId
        solution' input `shouldBe` Right expected
      Nothing -> expectationFailure "Type mismatch between function result and answer."

testN :: SolutionId -> Int -> Expectation
testN = test

testL :: SolutionId -> Integer -> Expectation
testL = test

testW :: SolutionId -> Word16 -> Expectation
testW = test

testS :: SolutionId -> String -> Expectation
testS = test

main :: IO ()
main = hspec . parallel $ do
  describe "Year 2015" $ do
    it "Day 01.1" $ do testN (2015, 01, 1) 138
    it "Day 01.2" $ do testN (2015, 01, 2) 1771
    it "Day 02.1" $ do testN (2015, 02, 1) 1586300
    it "Day 02.2" $ do testN (2015, 02, 2) 3737498
    it "Day 03.1" $ do testN (2015, 03, 1) 2592
    it "Day 03.2" $ do testN (2015, 03, 2) 2360
    it "Day 04.1" $ do testN (2015, 04, 1) 282749
    it "Day 04.2" $ do testN (2015, 04, 2) 9962624
    it "Day 05.1" $ do testN (2015, 05, 1) 238
    it "Day 05.2" $ do testN (2015, 05, 2) 69
    it "Day 06.1" $ do testN (2015, 06, 1) 569999
    it "Day 06.2" $ do testN (2015, 06, 2) 17836115
    it "Day 07.1" $ do testW (2015, 07, 1) 3176
    it "Day 07.2" $ do testW (2015, 07, 2) 14710
    it "Day 08.1" $ do testN (2015, 08, 1) 1350
    it "Day 08.2" $ do testN (2015, 08, 2) 2085
    it "Day 09.1" $ do testN (2015, 09, 1) 207
    it "Day 09.2" $ do testN (2015, 09, 2) 804
    it "Day 10.1" $ do testN (2015, 10, 1) 252594
    it "Day 10.2" $ do testN (2015, 10, 2) 3579328
    it "Day 11.1" $ do testS (2015, 11, 1) "vzbxxyzz"
    it "Day 11.2" $ do testS (2015, 11, 2) "vzcaabcc"
    it "Day 12.1" $ do testN (2015, 12, 1) 191164
    it "Day 12.2" $ do testN (2015, 12, 2) 87842
    it "Day 13.1" $ do testN (2015, 13, 1) 733
    it "Day 13.2" $ do testN (2015, 13, 2) 725
    it "Day 14.1" $ do testN (2015, 14, 1) 2640
    it "Day 14.2" $ do testN (2015, 14, 2) 1102
  describe "Year 2019" $ do
    it "Day 01.1" $ do testN (2019, 01, 1) 3184233
    it "Day 01.2" $ do testN (2019, 01, 2) 4773483
    it "Day 02.1" $ do testN (2019, 02, 1) 3409710
    it "Day 02.2" $ do testN (2019, 02, 2) 7912
  describe "Year 2024" $ do
    it "Day 01.1" $ do testN (2024, 01, 1) 2769675
    it "Day 01.2" $ do testN (2024, 01, 2) 24643097
    it "Day 02.1" $ do testN (2024, 02, 1) 490
    it "Day 02.2" $ do testN (2024, 02, 2) 536
    it "Day 03.1" $ do testN (2024, 03, 1) 173517243
    it "Day 03.2" $ do testN (2024, 03, 2) 100450138
    it "Day 04.1" $ do testN (2024, 04, 1) 2603
    it "Day 04.2" $ do testN (2024, 04, 2) 1965
    it "Day 05.1" $ do testN (2024, 05, 1) 4814
    it "Day 05.2" $ do testN (2024, 05, 2) 5448
  describe "Year 2025" $ do
    it "Day 01.1" $ do testN (2025, 01, 1) 1026
    it "Day 01.2" $ do testN (2025, 01, 2) 5923
    it "Day 02.1" $ do testL (2025, 02, 1) 8576933996
    it "Day 02.2" $ do testL (2025, 02, 2) 25663320831
    it "Day 03.1" $ do testL (2025, 03, 1) 17443
    it "Day 03.2" $ do testL (2025, 03, 2) 172167155440541
    it "Day 04.1" $ do testN (2025, 04, 1) 1508
    it "Day 04.2" $ do testN (2025, 04, 2) 8538
    it "Day 05.1" $ do testN (2025, 05, 1) 674
    it "Day 05.2" $ do testL (2025, 05, 2) 352509891817881
    it "Day 06.1" $ do testL (2025, 06, 1) 4951502530386
    it "Day 06.2" $ do testL (2025, 06, 2) 8486156119946
