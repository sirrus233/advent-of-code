module Solutions.Y2024.Day04 (solution1, solution2) where

-- https://adventofcode.com/2024/day/4

import Advent (Parser, Solution)
import Data.Foldable (maximum)
import Data.HashMap.Strict (lookup)
import Data.HashMap.Strict qualified as Map
import Text.Megaparsec (anySingle, manyTill, parse)
import Text.Megaparsec.Char (char)

type Point = (Int, Int)

type WordSearch = HashMap Point Char

data Direction = DUp | DDown | DLeft | DRight | DUpLeft | DUpRight | DDownLeft | DDownRight

parser :: Parser WordSearch
parser = toGrid <$> many (manyTill anySingle (char '\n'))
  where
    toGrid :: [[Char]] -> WordSearch
    toGrid cs = fromList $ zip [0 ..] (zip [0 ..] <$> cs) >>= uncurry (\y -> map (\(x, c) -> ((x, y), c)))

nextPoint :: Direction -> Point -> Point
nextPoint DUp (x, y) = (x, y - 1)
nextPoint DDown (x, y) = (x, y + 1)
nextPoint DLeft (x, y) = (x - 1, y)
nextPoint DRight (x, y) = (x + 1, y)
nextPoint DUpLeft (x, y) = nextPoint DLeft . nextPoint DUp $ (x, y)
nextPoint DUpRight (x, y) = nextPoint DRight . nextPoint DUp $ (x, y)
nextPoint DDownLeft (x, y) = nextPoint DLeft . nextPoint DDown $ (x, y)
nextPoint DDownRight (x, y) = nextPoint DRight . nextPoint DDown $ (x, y)

nextChar :: Char -> Char
nextChar 'X' = 'M'
nextChar 'M' = 'A'
nextChar 'A' = 'S'
nextChar _ = '_'

searchXmas :: WordSearch -> Point -> Int
searchXmas ws point = case lookup point ws of
  Nothing -> 0
  Just c | c == 'X' -> sum . map (go point 'X') $ [DUp, DDown, DLeft, DRight, DUpLeft, DUpRight, DDownLeft, DDownRight]
  _ -> 0
  where
    go _ 'S' _ = 1
    go p c d = case lookup p' ws of
      Nothing -> 0
      Just c' | c' == nextChar c -> go p' c' d
      _ -> 0
      where
        p' = nextPoint d p

searchMasX :: WordSearch -> Point -> Int
searchMasX ws point = case lookup point ws of
  Nothing -> 0
  Just c | c == 'A' && isMas diag1 && isMas diag2 -> 1
  _ -> 0
  where
    diag1 = (lookup (nextPoint DUpLeft point) ws, lookup (nextPoint DDownRight point) ws)
    diag2 = (lookup (nextPoint DDownLeft point) ws, lookup (nextPoint DUpRight point) ws)
    isMas pair = (pair == (Just 'M', Just 'S')) || (pair == (Just 'S', Just 'M'))

countWords :: (WordSearch -> Point -> Int) -> WordSearch -> Int
countWords f ws = sum . map (f ws) $ allPoints
  where
    (maxX, maxY) = maximum $ Map.keys ws
    allPoints = [(x, y) | x <- [0 .. maxX], y <- [0 .. maxY]]

solution1 :: Solution Int
solution1 input = countWords searchXmas <$> parse parser "" input

solution2 :: Solution Int
solution2 input = countWords searchMasX <$> parse parser "" input
