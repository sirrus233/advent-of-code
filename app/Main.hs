module Main where

import Advent (Day, Problem, SolutionId, Year, readInput)
import Control.Exception (try)
import Data.HashMap.Strict qualified as Map
import Data.Text (Text)
import NewSolution (newSolution)
import Options.Applicative (Parser, ParserInfo, ReadM, argument, auto, execParser, fullDesc, helper, info, long, metavar, readerError, short, switch, (<**>))
import Solutions.All (solutions)

data Opts = Opts {year :: Year, day :: Day, problem :: Problem, new :: Bool} deriving (Show)

maxYear :: Year
maxYear = 2024

optsParser :: ParserInfo Opts
optsParser = info (parseOpts <**> helper) fullDesc
  where
    parseOpts :: Parser Opts
    parseOpts =
      Opts
        <$> parseIntArg "YEAR" 2015 maxYear
        <*> parseIntArg "DAY" 1 25
        <*> parseIntArg "SOLUTION" 1 2
        <*> switch (long "new" <> short 'n')

    parseIntArg :: String -> Int -> Int -> Parser Int
    parseIntArg name min' max' = argument readInt (metavar name)
      where
        readInt :: ReadM Int
        readInt = auto >>= \i -> if i >= min' && i <= max' then pure i else readerError msg
          where
            msg = name <> " should be in the range " <> show min' <> "-" <> show max'

printSolvedProblem :: SolutionId -> IO ()
printSolvedProblem solutionId = do
  case Map.lookup solutionId solutions of
    Nothing -> print $ "No solution found for " <> show solutionId
    Just solution -> do
      inputOrExc <- try $ readInput solutionId :: IO (Either IOError Text)
      case inputOrExc of
        Left err -> print err
        Right input -> case solution input of
          Left err -> print err
          Right n -> print n

main :: IO ()
main = do
  opts <- execParser optsParser
  let solutionId = (opts.year, opts.day, opts.problem)
  if opts.new then newSolution solutionId else printSolvedProblem solutionId
