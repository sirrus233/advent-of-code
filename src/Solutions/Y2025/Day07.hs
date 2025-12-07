module Solutions.Y2025.Day07 (solution1, solution2) where

-- https://adventofcode.com/2025/day/7

import Advent (Parser, Solution, lexeme)
import Data.Foldable1 (maximum)
import Data.HashMap.Strict qualified as Map
import Data.HashSet qualified as Set
import Data.List.NonEmpty qualified as NE
import Data.Maybe (fromJust)
import Relude.Extra (firstF)
import Text.Megaparsec (parse)
import Text.Megaparsec.Char (char)

type Beam = (Int, Int)

type Start = Beam

type End = Int

type Splitters = HashSet Beam

type Splits = Int

type Timelines = Integer

type Memo = HashMap Beam Timelines

parser :: Parser (Start, End, Splitters)
parser = mk . join . NE.zipWith (\y xs -> firstF (,y) xs) [0 ..] <$> NE.some1 pLine
  where
    pChar = char '.' <|> char 'S' <|> char '^'
    pLine = NE.zip [0 ..] <$> lexeme (NE.some1 pChar)
    mk grid = (start, end, splitters)
      where
        start = fst . fromJust . find ((==) 'S' . snd) $ grid
        end = maximum . fmap (snd . fst) $ grid
        splitters = fromList . fmap fst . NE.filter ((==) '^' . snd) $ grid

beamManifold :: (Start, End, Splitters) -> Splits
beamManifold (s, end, splitters) = go [s]
  where
    go :: HashSet Beam -> Splits
    go beams
      | all ((==) end . snd) beams = 0
      | otherwise = (\(ss, bs) -> sum ss + go (fromList . concat $ bs)) . unzip . fmap nextBeam . toList $ beams
    nextBeam :: Beam -> (Splits, [Beam])
    nextBeam (x, y)
      | (x, y + 1) `Set.member` splitters = (1, [(x - 1, y + 1), (x + 1, y + 1)])
      | otherwise = (0, [(x, y + 1)])

quantumBeamManifold :: (Start, End, Splitters) -> Timelines
quantumBeamManifold (s, end, splitters) = fst $ go s (fromList [])
  where
    go :: Beam -> Memo -> (Timelines, Memo)
    go beam@(x, y) memo = case Map.lookup beam memo of
      Just v -> (v, memo)
      Nothing
        | y == end -> (1, memo)
        | not . Set.member (x, y + 1) $ splitters -> go (x, y + 1) memo
        | otherwise -> (left + right, Map.insert beam (left + right) memo'')
        where
          (left, memo') = go (x - 1, y + 1) memo
          (right, memo'') = go (x + 1, y + 1) $ Map.insert beam left memo'

solution1 :: Solution Int
solution1 input = beamManifold <$> parse parser "" input

solution2 :: Solution Integer
solution2 input = quantumBeamManifold <$> parse parser "" input
