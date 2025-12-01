module Main where

import Advent (AnySolution (..), Day, Problem, SolutionId, Year, readInput)
import Control.Exception (IOException, try)
import Data.HashMap.Strict qualified as Map
import NewSolution (newSolution)
import Options.Applicative (Parser, ParserInfo, ReadM, execParser)
import Options.Applicative qualified as Opt
import Solutions.All (solutions)

data Opts = Opts {year :: Year, day :: Day, problem :: Problem, new :: Bool} deriving (Show)

maxYear :: Year
maxYear = 2025

optsParser :: ParserInfo Opts
optsParser = Opt.info (parseOpts <**> Opt.helper) Opt.fullDesc
  where
    parseOpts :: Parser Opts
    parseOpts =
      Opts
        <$> parseIntArg "YEAR" 2015 maxYear
        <*> parseIntArg "DAY" 1 25
        <*> parseIntArg "SOLUTION" 1 2
        <*> Opt.switch (Opt.long "new" <> Opt.short 'n')

    parseIntArg :: String -> Int -> Int -> Parser Int
    parseIntArg name min' max' = Opt.argument readInt (Opt.metavar name)
      where
        readInt :: ReadM Int
        readInt = Opt.auto >>= \i -> if i >= min' && i <= max' then pure i else Opt.readerError msg
          where
            msg = name <> " should be in the range " <> show min' <> "-" <> show max'

printSolvedProblem :: SolutionId -> IO ()
printSolvedProblem solutionId = do
  case Map.lookup solutionId solutions of
    Nothing -> print $ "No solution found for " <> show @Text solutionId
    Just (AnySolution solution) -> do
      inputOrExc <- try $ readInput solutionId :: IO (Either IOException Text)
      case inputOrExc of
        Left err -> print err
        Right input -> case solution input of
          Left err -> print err
          Right n -> putStrLn (show n :: String)

main :: IO ()
main = do
  opts <- execParser optsParser
  let solutionId = (opts.year, opts.day, opts.problem)
  if opts.new then newSolution solutionId else printSolvedProblem solutionId
