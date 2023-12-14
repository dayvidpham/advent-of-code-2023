module Ranges where

import System.IO
import Data.Set (Set)
import qualified Data.Set as Set

import Data.Map (Map)
import qualified Data.Map as Map

isNumber :: Char -> Bool
isNumber c = c `elem` "0123456789"

isSymbol :: Char -> Bool
isSymbol c = not $ c `elem` "0123456789."

hasSymbol :: String -> Bool
hasSymbol ln = case dropWhile notSymbol ln of
    "" -> False
    _  -> True
    where notSymbol c = not $ isSymbol c

indexOf :: (Eq a) => a -> [a] -> Int -> Int
indexOf y xs idx = case xs of
    []      -> -1
    (x:xss) -> case y == x of
        True  -> idx
        False -> indexOf y xss (idx+1)



-- Staged input parsing
-- dunno if can do in one shot?
-- don't want to hard code line number grabbing: cmon bruh
-- stage 1: 
--  * simply grab seeds
--  * consume empty line underneath
-- stage 2:
--  * contained in a function
--  * consumes lines until an empty line or EOF encountered?
-- 
-- Create a datatype to ease working with ranges
mkTupFromInts :: [Int] -> (Int, Int, Int)
mkTupFromInts (x1:x2:x3:_) = (x1,x2,x3) 

putRangesFromLns :: [String] -> Map (Int, Int) (Int, Int) -> ([String], Map (Int, Int) (Int, Int))
putRangesFromLns lns mp 
    | null lns  = ([], mp)      -- done: no more lines
    | null ln   = (lnss, mp)    -- done: encounter empty line
    | otherwise = case isNumber c of
        False -> putRangesFromLns lnss mp -- skip line not starting with number
        True  -> putRangesFromLns lnss nextMp
            where (dstA, srcA, len) = mkTupFromInts . mkIntsFromLn $ ln
                  nextMp = Map.insert (srcA, srcA+len) (dstA, dstA+len) mp
    where (ln:lnss) = lns
          (c:cs) = ln

mkIntsFromLn :: String -> [Int]
mkIntsFromLn ln = map read $ words ln :: [Int]

main :: IO ()
main = let
    inputPath   = "../5-input.txt";
  in do
    inputText   <- readFile inputPath
    let lns     = lines inputText
        (seedStr:_:mapLns) = lns
        (_:seeds)   = mkIntsFromLn seedStr

    print seedStr
