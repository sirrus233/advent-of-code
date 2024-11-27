module Solutions.All where

import Advent (AnySolution (..), SolutionId)
import Solutions.Y2015.Day01 qualified as Y2015_01
import Solutions.Y2015.Day02 qualified as Y2015_02
import Solutions.Y2015.Day03 qualified as Y2015_03
import Solutions.Y2015.Day04 qualified as Y2015_04
import Solutions.Y2015.Day05 qualified as Y2015_05
import Solutions.Y2015.Day06 qualified as Y2015_06
import Solutions.Y2015.Day07 qualified as Y2015_07
import Solutions.Y2015.Day08 qualified as Y2015_08
import Solutions.Y2015.Day09 qualified as Y2015_09
import Solutions.Y2015.Day10 qualified as Y2015_10
import Solutions.Y2015.Day11 qualified as Y2015_11
import Solutions.Y2019.Day01 qualified as Y2019_01
import Solutions.Y2019.Day02 qualified as Y2019_02

solutions :: HashMap SolutionId AnySolution
solutions =
  [ ((2015, 01, 1), AnySolution Y2015_01.solution1),
    ((2015, 01, 2), AnySolution Y2015_01.solution2),
    ((2015, 02, 1), AnySolution Y2015_02.solution1),
    ((2015, 02, 2), AnySolution Y2015_02.solution2),
    ((2015, 03, 1), AnySolution Y2015_03.solution1),
    ((2015, 03, 2), AnySolution Y2015_03.solution2),
    ((2015, 04, 1), AnySolution Y2015_04.solution1),
    ((2015, 04, 2), AnySolution Y2015_04.solution2),
    ((2015, 05, 1), AnySolution Y2015_05.solution1),
    ((2015, 05, 2), AnySolution Y2015_05.solution2),
    ((2015, 06, 1), AnySolution Y2015_06.solution1),
    ((2015, 06, 2), AnySolution Y2015_06.solution2),
    ((2015, 07, 1), AnySolution Y2015_07.solution1),
    ((2015, 07, 2), AnySolution Y2015_07.solution2),
    ((2015, 08, 1), AnySolution Y2015_08.solution1),
    ((2015, 08, 2), AnySolution Y2015_08.solution2),
    ((2015, 09, 1), AnySolution Y2015_09.solution1),
    ((2015, 09, 2), AnySolution Y2015_09.solution2),
    ((2015, 10, 1), AnySolution Y2015_10.solution1),
    ((2015, 10, 2), AnySolution Y2015_10.solution2),
    ((2015, 11, 1), AnySolution Y2015_11.solution1),
    ((2015, 11, 2), AnySolution Y2015_11.solution2),
    ((2019, 01, 1), AnySolution Y2019_01.solution1),
    ((2019, 01, 2), AnySolution Y2019_01.solution2),
    ((2019, 02, 1), AnySolution Y2019_02.solution1),
    ((2019, 02, 2), AnySolution Y2019_02.solution2)
  ]