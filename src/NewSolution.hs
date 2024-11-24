module NewSolution (newSolution) where

import Advent (SolutionId, dayString, getInputFilename)
import Data.Text (breakOn)
import Network.HTTP.Simple (addRequestHeader, getResponseBody, httpBS, parseRequest)
import System.Directory (listDirectory)
import Text.Printf (printf)

getSession :: IO ByteString
getSession = readFileBS "session"

problemUrl :: SolutionId -> String
problemUrl (y, d, _) = printf "https://adventofcode.com/%d/day/%d" y d

getPuzzleInput :: SolutionId -> IO Text
getPuzzleInput solutionId = do
  session <- getSession
  request <- addRequestHeader "Cookie" ("session=" <> session) <$> parseRequest ("GET " <> url)
  response <- httpBS request
  pure . decodeUtf8 . getResponseBody $ response
  where
    url = problemUrl solutionId <> "/input"

writeInput :: SolutionId -> IO ()
writeInput solutionId = getPuzzleInput solutionId >>= writeFileText path
  where
    path = getInputFilename solutionId

writeTemplate :: SolutionId -> IO ()
writeTemplate solutionId@(y, d, _) = writeFile path template
  where
    path = printf "src/Solutions/Y%d/Day%s.hs" y (dayString d)
    template = moduleHeader <> commentHeader <> imports <> parser <> solutionFunction 1 <> "\n" <> solutionFunction 2

    moduleHeader = printf "module Solutions.Y%d.Day%s (solution1, solution2) where\n\n" y (dayString d)
    commentHeader = "-- " <> problemUrl solutionId <> "\n\n"
    imports =
      "import Advent (Parser, Solution)\n\
      \import Control.Applicative.Combinators.NonEmpty qualified as NE\n\
      \import Data.List.NonEmpty qualified as NE\n\
      \import Text.Megaparsec (parse)\n\n"
    parser = "parser :: Parser Int\nparser = pure 0\n\n"
    solutionFunction :: Int -> String
    solutionFunction n =
      printf "solution%d :: Solution Int\nsolution%d input = const 0 <$> parse parser \"\" input\n" n n

getExistingSolutions :: IO [SolutionId]
getExistingSolutions = do
  solutionsSubDirs <- filter (isPrefixOf "Y2") <$> listDirectory solutionsDir
  solutionModules <- traverse (listDirectory . mappend solutionsDir) solutionsSubDirs
  let years = map (fromMaybe 0 . readMaybe . drop 1) solutionsSubDirs
  let days = sort . map (fromMaybe 0 . readMaybe . take 2 . drop 3) <$> solutionModules
  let ss = sort $ zip years days
  pure $ concat [[(y, d, 1), (y, d, 2)] | (y, ds) <- ss, d <- ds]
  where
    solutionsDir = "src/Solutions/"

writeRegistry :: IO ()
writeRegistry = getExistingSolutions >>= writeFile path . registryModule
  where
    path = "src/Solutions/All.hs"
    solutionsDict ss = dictPre <> dictEntries ss <> dictPost
    registryModule ss = moduleHeader <> adventImport <> solutionImports ss <> "\n" <> solutionsDict ss

    moduleHeader = "module Solutions.All where\n\n"
    adventImport = "import Advent (AnySolution (..), SolutionId)\n"
    solutionImports = concat . hashNub . map solutionImport
    dictPre = "solutions :: HashMap SolutionId AnySolution\nsolutions =\n  [ "
    dictEntries = intercalate ",\n    " . map dictEntry
    dictPost = "\n  ]"

    qualifier :: SolutionId -> String
    qualifier (y, d, _) = printf "Y%d_%s" y (dayString d)
    solutionImport :: SolutionId -> String
    solutionImport solutionId@(y, d, _) =
      printf "import Solutions.Y%d.Day%s qualified as %s\n" y (dayString d) (qualifier solutionId)
    dictEntry :: SolutionId -> String
    dictEntry solutionId@(y, d, p) =
      printf "((%d, %s, %d), AnySolution %s.solution%d)" y (dayString d) p (qualifier solutionId) p

writeCabal :: SolutionId -> IO ()
writeCabal (y, d, _) = readFileBS path >>= (writeFileText path . cabalContents) . decodeUtf8
  where
    path = "advent-of-code.cabal"
    newEntry = toText $ replicate 22 ' ' <> printf "Solutions.Y%d.Day%s,\n" y (dayString d)
    cabalContents = (\(pre, post) -> pre <> newEntry <> post) . breakOn "    hs-source-dirs:   src"

newSolution :: SolutionId -> IO ()
newSolution solutionId = do
  writeInput solutionId
  writeTemplate solutionId
  writeRegistry
  writeCabal solutionId
