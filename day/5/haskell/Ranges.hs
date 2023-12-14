module Ranges where

import System.IO
import Data.Set (Set)
import qualified Data.Set as Set

import Data.Map (Map)
import qualified Data.Map as Map

import qualified Data.List as List

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


mkIntsFromLn :: String -> [Int]
mkIntsFromLn ln = case isNumber c of
    False -> map read ws :: [Int]
    True  -> map read splits :: [Int]
    where splits = words ln
          (w:ws) = splits
          (c:_) = w
          

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


mkDictsFromLns :: [String] -> [Map (Int, Int) (Int, Int)] -> [Map (Int, Int) (Int, Int)]
mkDictsFromLns lns mps = case lns of
    [] -> reverse mps
    _  -> mkDictsFromLns rem (mp:mps)
    where (rem, mp) = putRangesFromLns lns Map.empty


findDest :: Int -> [((Int, Int), (Int, Int))] -> Int
findDest x rgs = case pivot of
        []  -> x
        ((src, dst):_)  | x >= xBgn -> yBgn + offset
                        | x < xBgn  -> x
            where (xBgn, xEnd) = src
                  (yBgn, yEnd) = dst
                  offset = x-xBgn
    where pivot = dropWhile (\ rg -> x >= (snd . fst $ rg)) rgs


main :: IO ()
main = let
    inputPath   = "../5-input.txt";
  in do
    inputText   <- readFile inputPath
    let lns     = lines inputText
        (seedStr:_:mapLns) = lns
        seeds   = mkIntsFromLn seedStr
        mps     = mkDictsFromLns mapLns []
        (one:_) = mps
    --putStrLn $ foldl (\ acm m -> concat [acm, (show $ Map.toAscList m), "\n\n"]) "" mps
    print $ findDest 4294967296 $ Map.toAscList one




