module Solutions.Y2019.Day02 (solution1, solution2) where

-- https://adventofcode.com/2019/day/2

import Advent (Parser, Solution)
import Control.Applicative.Combinators.NonEmpty qualified as NE
import Data.List.NonEmpty qualified as NE
import Data.Maybe (fromJust)
import Text.Megaparsec (parse)
import Text.Megaparsec.Char (char)
import Text.Megaparsec.Char.Lexer qualified as L

type Program = NonEmpty Int

type Noun = Int

type Verb = Int

type Address = Int

parser :: Parser Program
parser = L.decimal `NE.sepEndBy1` char ','

initialize :: Noun -> Verb -> Program -> Program
initialize noun verb (output :| _ : _ : program) = output :| noun : verb : program
initialize _ _ p = p

execProgram :: Program -> Program
execProgram = go 0
  where
    go :: Address -> Program -> Program
    go instr p = case NE.drop instr p of
      (99 : _) -> p
      (1 : a : b : out : _) -> go (instr + 4) (writeMem out (readMem a + readMem b))
      (2 : a : b : out : _) -> go (instr + 4) (writeMem out (readMem a * readMem b))
      _ -> p
      where
        readMem :: Address -> Int
        readMem = (NE.!!) p
        writeMem :: Address -> Int -> Program
        writeMem addr a = NE.prependList (NE.take addr p) (a :| NE.drop (addr + 1) p)

execAll :: Program -> NonEmpty Program
execAll p = fromList [execProgram . initialize a b $ p | a <- [0 .. 99], b <- [0 .. 99]]

findTarget :: Int -> NonEmpty Program -> Maybe Int
findTarget target programs = case NE.filter ((==) target . head) programs of
  [] -> Nothing
  ((_ :| n : v : _) : _) -> Just $ 100 * n + v
  _ -> Nothing

solution1 :: Solution Int
solution1 input = head . execProgram . initialize 12 2 <$> parse parser "" input

solution2 :: Solution Int
solution2 input = fromJust . findTarget 19690720 . execAll <$> parse parser "" input