import Ranges

import Data.Map (Map)
import qualified Data.Map as Map

inputPath   = "../5-input.txt";
inputText   = readFile inputPath

(_, mrg) = putRangesFromLns ["5 0 2"] Map.empty
lrg = Map.toAscList mrg
x = findDest 0 lrg
y = findDest 1 lrg
z = findDest 2 lrg
--let lns     = lines inputText
--    (seedStr:_:mapLns) = lns
--    (_:seeds)   = mkIntsFromLn seedStr

--main :: IO [Integer]
--main = lineToParts input []
