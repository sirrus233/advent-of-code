module NewSolution (newSolution) where

import Advent (SolutionId, dayString, getInputFilename)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Containers.ListUtils (nubOrd)
import Data.List (intercalate, isPrefixOf, sort)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding (decodeUtf8)
import Data.Text.IO.Utf8 qualified as TIO
import Network.HTTP.Simple (addRequestHeader, getResponseBody, httpBS, parseRequest)
import System.Directory (listDirectory)
import Text.Printf (printf)

getSession :: IO ByteString
getSession = BS.readFile "session"

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
writeInput solutionId = getPuzzleInput solutionId >>= TIO.writeFile path
  where
    path = getInputFilename solutionId

writeTemplate :: SolutionId -> IO ()
writeTemplate solutionId@(y, d, _) = writeFile path template
  where
    path = printf "src/Solutions/Y%d/Day%s.hs" y (dayString d)
    template = moduleHeader <> commentHeader <> imports <> parser <> solutionFunction 1 <> solutionFunction 2

    moduleHeader = printf "module Solutions.Y%d.Day%s (solution1, solution2) where\n\n" y (dayString d)
    commentHeader = "-- " <> problemUrl solutionId <> "\n\n"
    imports = "import Advent (Parser, Solution)\nimport Text.Megaparsec (parse)\n\n"
    parser = "parser :: Parser Integer\nparser = pure 0\n\n"
    solutionFunction :: Int -> String
    solutionFunction n =
      printf "solution%d :: Solution\nsolution%d input = 0 <$ parse parser \"\" input\n" n n

getExistingSolutions :: IO [SolutionId]
getExistingSolutions = do
  solutionsSubDirs <- filter (isPrefixOf "Y2") <$> listDirectory solutionsDir
  solutionModules <- traverse (listDirectory . mappend solutionsDir) solutionsSubDirs
  let years = map (read . drop 1) solutionsSubDirs
  let days = sort . map (read . take 2 . drop 3) <$> solutionModules
  let ss = sort $ zip years days
  pure $ concat [[(y, d, 1), (y, d, 2)] | (y, ds) <- ss, d <- ds]
  where
    solutionsDir = "src/Solutions/"

writeRegistry :: IO ()
writeRegistry = getExistingSolutions >>= writeFile path . registryModule
  where
    path = "src/Solutions/All.hs"
    solutionsDict ss = dictPre <> dictEntries ss <> dictPost
    registryModule ss = moduleHeader <> imports <> solutionImports ss <> "\n" <> solutionsDict ss

    moduleHeader = "module Solutions.All where\n\n"
    imports = "import Advent (Solution, SolutionId)\nimport Data.HashMap.Strict (HashMap)\n"
    solutionImports = concat . nubOrd . map solutionImport
    dictPre = "solutions :: HashMap SolutionId Solution\nsolutions =\n  [ "
    dictEntries = intercalate ",\n    " . map dictEntry
    dictPost = "\n  ]"

    qualifier :: SolutionId -> String
    qualifier (y, d, _) = printf "Y%d_%s" y (dayString d)
    solutionImport :: SolutionId -> String
    solutionImport solutionId@(y, d, _) =
      printf "import Solutions.Y%d.Day%s qualified as %s\n" y (dayString d) (qualifier solutionId)
    dictEntry :: SolutionId -> String
    dictEntry solutionId@(y, d, p) =
      printf "((%d, %s, %d), %s.solution%d)" y (dayString d) p (qualifier solutionId) p

writeCabal :: SolutionId -> IO ()
writeCabal (y, d, _) = TIO.readFile path >>= TIO.writeFile path . cabalContents
  where
    path = "advent-of-code.cabal"
    newEntry = T.pack $ replicate 22 ' ' <> printf "Solutions.Y%d.Day%s,\n" y (dayString d)
    cabalContents = (\(pre, post) -> pre <> newEntry <> post) . T.breakOn "    hs-source-dirs:   src"

newSolution :: SolutionId -> IO ()
newSolution solutionId = do
  writeInput solutionId
  writeTemplate solutionId
  writeRegistry
  writeCabal solutionId
