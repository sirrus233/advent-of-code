module Solutions.Y2024.Day02 (solution1, solution2) where

-- https://adventofcode.com/2024/day/2

import Advent (Parser, Solution)
import Text.Megaparsec (parse, sepEndBy1)
import Text.Megaparsec.Char (char)
import Text.Megaparsec.Char.Lexer qualified as L

parser :: Parser [[Int]]
parser = (L.decimal `sepEndBy1` char ' ') `sepEndBy1` char '\n'

isSafe :: [Int] -> Bool
isSafe = liftA2 (&&) isSafeMagnitude isSafeDirection . (zip <*> drop 1)
  where
    isSafeMagnitude = all (\(a, b) -> a /= b && abs (a - b) <= 3)
    isSafeDirection = (||) <$> all (uncurry (<)) <*> all (uncurry (>))

possibilities :: [Int] -> [[Int]]
possibilities xs = xs : [(\(as, bs) -> as <> drop 1 bs) $ splitAt i xs | i <- [0 .. length xs - 1]]

solution1 :: Solution Int
solution1 input = length . filter isSafe <$> parse parser "" input

solution2 :: Solution Int
solution2 input = length . filter (any isSafe) . map possibilities <$> parse parser "" input
