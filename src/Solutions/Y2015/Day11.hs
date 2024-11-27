module Solutions.Y2015.Day11 (solution1, solution2) where

-- https://adventofcode.com/2015/day/11

import Advent (Parser, Solution)
import Relude.Extra (next)
import Text.Megaparsec (parse)
import Text.Megaparsec.Char (letterChar)

parser :: Parser Text
parser = toText <$> some letterChar

nextChar :: Char -> Char
nextChar 'h' = 'j'
nextChar 'k' = 'm'
nextChar 'n' = 'p'
nextChar 'z' = 'a'
nextChar c = next c

nextPassword :: String -> String
nextPassword = reverse . increment . reverse
  where
    increment [] = []
    increment (x : xs)
      | x == 'z' = nextChar x : increment xs
      | otherwise = nextChar x : xs

findValidPassword :: String -> String
findValidPassword pwd
  | hasStraight && hasPairs = pwd
  | otherwise = findValidPassword . nextPassword $ pwd
  where
    hasStraight = any (\(a, b, c) -> next a == b && next b == c) $ zip3 pwd (drop 1 pwd) (drop 2 pwd)
    hasPairs = (>= 2) . length . filter ((>= 2) . length) . group $ pwd

solution1 :: Solution String
solution1 input = findValidPassword . toString <$> parse parser "" input

solution2 :: Solution String
solution2 input = findValidPassword . nextPassword . findValidPassword . toString <$> parse parser "" input
