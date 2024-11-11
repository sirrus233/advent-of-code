module Solutions.Y2015.Day05 (solution1, solution2) where

-- https://adventofcode.com/2015/day/5

import Advent (Parser, Solution, length')
import Control.Applicative.Combinators.NonEmpty (sepEndBy1, some)
import Control.Monad (foldM)
import Data.Either (isLeft)
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as Map
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NE
import Text.Megaparsec (anySingleBut, eof, parse)
import Text.Megaparsec.Char (eol)

type ChPair = (Char, Char)

parser :: Parser (NonEmpty (NonEmpty Char))
parser = some (anySingleBut '\n') `sepEndBy1` eol <* eof

isNice1 :: NonEmpty Char -> Bool
isNice1 str = threeVowels str && doubleLetter str && not (hasForbidden str)
  where
    threeVowels = (<=) 3 . length . NE.filter (`elem` ("aeiou" :: [Char]))
    doubleLetter s = any (uncurry (==)) $ zip (NE.toList s) (NE.tail s)
    hasForbidden s = any (`elem` forbidden) $ zip (NE.toList s) (NE.tail s)
      where
        forbidden = [('a', 'b'), ('c', 'd'), ('p', 'q'), ('x', 'y')] :: [ChPair]

isNice2 :: NonEmpty Char -> Bool
isNice2 str = duplicatedPair str && separatedDouble str
  where
    duplicatedPair s = isLeft . foldM nextPair Map.empty . zip [0 ..] . zip (NE.toList s) $ NE.tail s
      where
        nextPair :: HashMap ChPair Int -> (Int, ChPair) -> Either () (HashMap ChPair Int)
        nextPair ps (idx, p) = case Map.lookup p ps of
          Nothing -> Right $ Map.insert p idx ps
          Just prevIdx
            | idx - prevIdx > 1 -> Left ()
            | otherwise -> Right ps
    separatedDouble s = any (uncurry (==)) $ zip (NE.toList s) (NE.drop 2 s)

solution1 :: Solution
solution1 input = length' . NE.filter isNice1 <$> parse parser "" input

solution2 :: Solution
solution2 input = length' . NE.filter isNice2 <$> parse parser "" input
