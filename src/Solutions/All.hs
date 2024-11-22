module Solutions.All where

import Advent (Solution, SolutionId)
import Solutions.Y2015.Day01 qualified as Y2015_01
import Solutions.Y2015.Day02 qualified as Y2015_02
import Solutions.Y2015.Day03 qualified as Y2015_03
import Solutions.Y2015.Day04 qualified as Y2015_04
import Solutions.Y2015.Day05 qualified as Y2015_05
import Solutions.Y2015.Day06 qualified as Y2015_06
import Solutions.Y2015.Day07 qualified as Y2015_07

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
    ((2015, 05, 2), Y2015_05.solution2),
    ((2015, 06, 1), Y2015_06.solution1),
    ((2015, 06, 2), Y2015_06.solution2),
    ((2015, 07, 1), Y2015_07.solution1),
    ((2015, 07, 2), Y2015_07.solution2)
  ]