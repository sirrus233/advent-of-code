module Solutions.Y2025.Day02 (solution1, solution2) where

-- https://adventofcode.com/2025/day/2

import Advent (Parser, Solution)
import Data.Ix (inRange)
import Data.List.NonEmpty qualified as NE
import Data.Text qualified as T
import Relude.Extra (fmapToSnd)
import Text.Megaparsec (parse)
import Text.Megaparsec.Char (char)
import Text.Megaparsec.Char.Lexer (decimal)

type Range = (Integer, Integer)

minWithDigits :: (Integral a, Num b) => a -> b
minWithDigits i = 10 ^ (i - 1)

maxWithDigits :: (Integral a, Num b) => a -> b
maxWithDigits i = (10 ^ i) - 1

mkRanges :: Integer -> Integer -> Either String (NonEmpty Range)
mkRanges a b
  | aSize < bSize = Right $ startRange :| midRanges <> [endRange]
  | aSize > bSize = Left $ "End bound " <> show b <> " before start bound " <> show a
  | otherwise = Right [(a, b)]
  where
    aSize = T.length (show @Text a)
    bSize = T.length (show @Text b)
    startRange = (a, maxWithDigits aSize)
    midRanges = map (\i -> (minWithDigits i, maxWithDigits i)) [aSize + 1 .. bSize - 1]
    endRange = (minWithDigits bSize, b)

parser :: Parser (NonEmpty Range)
parser = join <$> NE.some1 (pRanges >>= either fail pure)
  where
    pRanges = mkRanges <$> (decimal <* char '-') <*> (decimal <* (char ',' <|> char '\n'))

invalidIds :: Range -> [Integer]
invalidIds (a, b)
  | odd aSize = []
  | otherwise = [invalidId i | i <- [aFront .. bFront], inRange (a, b) (invalidId i)]
  where
    aSize = T.length (show @Text a)
    base = 10 ^ (aSize `div` 2)
    aFront = a `div` base
    bFront = b `div` base
    invalidId i = i * base + i

invalidIds' :: Range -> [Integer]
invalidIds' (a, b)
  | aSize == 1 = []
  | otherwise =
      ordNub [invalidId pre reps | (ds, reps) <- idFormats, pre <- idPrefixes ds, inRange (a, b) (invalidId pre reps)]
  where
    aSize = T.length (show @Text a)
    idFormats = fmapToSnd (div aSize) . filter (\i -> aSize `mod` i == 0) $ [1 .. aSize `div` 2]
    idPrefixes i = [minWithDigits i .. maxWithDigits i] :: [Integer]
    invalidId prefix reps = fromMaybe 0 . readMaybe @Integer . concat . replicate reps $ show prefix

solution1 :: Solution Integer
solution1 input = sum . concatMap invalidIds <$> parse parser "" input

solution2 :: Solution Integer
solution2 input = sum . concatMap invalidIds' <$> parse parser "" input
