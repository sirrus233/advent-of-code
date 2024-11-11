module Solutions.All where

import Advent (Solution, SolutionId)
import Data.HashMap.Strict (HashMap)
import Solutions.Y2015.Day01 qualified as Y2015_01
import Solutions.Y2015.Day02 qualified as Y2015_02
import Solutions.Y2015.Day03 qualified as Y2015_03
import Solutions.Y2015.Day04 qualified as Y2015_04
import Solutions.Y2015.Day05 qualified as Y2015_05

solutions :: HashMap SolutionId Solution
solutions =
  [ ((2015, 01, 1), Y2015_01.solution1),
    ((2015, 01, 2), Y2015_01.solution2),
    ((2015, 02, 1), Y2015_02.solution1),
    ((2015, 02, 2), Y2015_02.solution2),
    ((2015, 03, 1), Y2015_03.solution1),
    ((2015, 03, 2), Y2015_03.solution2),
    ((2015, 04, 1), Y2015_04.solution1),
    ((2015, 04, 2), Y2015_04.solution2),
    ((2015, 05, 1), Y2015_05.solution1),
    ((2015, 05, 2), Y2015_05.solution2)
  ]