module Solutions.Y2025.Day04 (solution1, solution2) where

-- https://adventofcode.com/2025/day/4

import Advent (Parser, Solution, lexeme)
import Data.HashSet qualified as Set
import Text.Megaparsec (parse)
import Text.Megaparsec.Char (char)

type Point = (Int, Int)

parser :: Parser [Point]
parser = concatMap catMaybes . zipWith (\y xs -> (,y) <<$>> xs) [0 ..] <$> some pLine
  where
    pChar = (Just <$ char '@') <|> (const Nothing <$ char '.')
    pLine = zipWith (flip ($)) [0 ..] <$> lexeme (some pChar)

neighbors :: Point -> NonEmpty Point
neighbors (a, b) =
  [ (a + 1, b + 1),
    (a + 1, b),
    (a + 1, b - 1),
    (a, b + 1),
    (a, b - 1),
    (a - 1, b + 1),
    (a - 1, b),
    (a - 1, b - 1)
  ]

isAccessible :: HashSet Point -> Point -> Bool
isAccessible ps = (< 4) . sum . fmap (fromEnum . (`Set.member` ps)) . neighbors

removeAccessible :: HashSet Point -> HashSet Point
removeAccessible ps
  | null accessible = ps
  | otherwise = removeAccessible $ Set.difference ps accessible
  where
    accessible = join (Set.filter . isAccessible) ps

solution1 :: Solution Int
solution1 input = sum . fmap fromEnum . join (fmap . (isAccessible . fromList)) <$> parse parser "" input

solution2 :: Solution Int
solution2 input = (\ps -> length ps - length (removeAccessible ps)) . fromList <$> parse parser "" input
