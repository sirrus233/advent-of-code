module Solutions.Y2025.Day01 (solution1, solution2) where

-- https://adventofcode.com/2025/day/1

import Advent (Parser, Solution, lexeme)
import Control.Applicative.Combinators.NonEmpty qualified as NE
import Text.Megaparsec (parse)
import Text.Megaparsec.Char (letterChar)
import Text.Megaparsec.Char.Lexer (decimal)

-- data Direction = R | L TODO

data Instruction = Instruction Char Int

type DialStart = Int

type DialEnd = Int

type IncrCount = Int

type Protocol = DialStart -> DialEnd -> IncrCount

parser :: Parser (NonEmpty Instruction)
parser = fmap (uncurry Instruction) <$> NE.some (lexeme ((,) <$> letterChar <*> decimal))

protocolSimple :: Protocol
protocolSimple _ end = fromEnum (end `mod` 100 == 0)

protocol0x434C49434B :: Protocol
protocol0x434C49434B start end
  | end == 0 = 1 + adjustments
  | otherwise = abs (end `div` 100) + adjustments
  where
    adjust1 = if start == 0 && end <= 0 then -1 else 0
    adjust2 = if end < 0 && end `mod` 100 == 0 then 1 else 0
    adjustments = adjust1 + adjust2

next :: Protocol -> (Int, Int) -> Instruction -> (Int, Int)
next protocol (p, zs) (Instruction c i) = (p' `mod` 100, zs')
  where
    p' = case c of
      'R' -> p + i
      'L' -> p - i
      _ -> error "Invalid Char" -- TODO
    zs' = zs + protocol p p'

solution1 :: Solution Int
solution1 input = snd . foldl' (next protocolSimple) (50, 0) <$> parse parser "" input

solution2 :: Solution Int
solution2 input = snd . foldl' (next protocol0x434C49434B) (50, 0) <$> parse parser "" input
