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


findDest :: Int -> Map (Int, Int) (Int, Int) -> Int
findDest x mp = case pivot of
        []  -> x
        ((src, dst):_)  | x >= xBgn -> yBgn + offset
                        | x < xBgn  -> x
            where (xBgn, xEnd) = src
                  (yBgn, yEnd) = dst
                  offset = x-xBgn
    where rgs = Map.toAscList mp
          pivot = dropWhile (\ rg -> x >= (snd . fst $ rg)) rgs


pipeSrcToDest :: Int -> [Map (Int, Int) (Int, Int)] -> Int
pipeSrcToDest x mps = foldl findDest x mps


dstToSrc :: Map (Int, Int) (Int, Int) -> Map (Int, Int) (Int, Int)
dstToSrc mp = Map.fromList $ map (\ (a,b) -> (b,a)) $ Map.toList mp

--overlapSplit :: (Int, Int) -> Map (Int, Int) (Int, Int) -> [(Int, Int)]
--overlapSplit rg mp = 

--splitOverlap :: (Int, Int) -> (Int, Int) -> [(Int, Int)]
--splitOverlap (xl, xr) (yl, yr) = case xl < yl of
--    
--    where 

spanSrcRgs :: (Int, Int) -> Map (Int, Int) (Int, Int) -> [(Int, Int)]
spanSrcRgs rg mp = concat [myl, Map.toAscList midMp, myr]
    where 
        (xl, xr) = rg
        (preMp, mmid, mps) = Map.splitLookup rg mp
        (midMp, _, postMp) = Map.splitLookup (xr, xr+1) mps
        yl = case (preMp, midMp) of
            (Map.empty, Map.empty) -> [(xl, xr)]
            (a, Map.empty) -> case xr <= arr of -- everything smaller than xl
                    True  -> [(xl, xr)]                  -- consumed
                    False -> [(xl, arr), (arr, xr)]     -- bifurcated
                where (arl, arr) = Map.findMax a
            (Map.empty, b) -> case xr <= bll of  -- everything bigger than xl
                    True  -> [(xl, xr)]
                    False -> [(xl, bll)] ++ (spanSrcRgs (bll, xr) b)
                where (bll, blr) = Map.findMin b
            (a, b)         -> [rg]
        
        yl = case preMp of
          Map.empty -> (xl, fst $ Map.findMin mps)
          _         -> case xl < yllr of 
                              False -> case midMp of
                                          Map.empty -> (xl, 
                              True  -> (xl, yllr)
              where yllr = Map.findMax preMp
        myr = Map.min postMp
          

--rgSrcToDst :: (Int, Int) -> Map (Int, Int) (Int, Int) -> Map (Int, Int) (Int, Int)



main :: IO ()
main = let
    inputPath   = "../5-input.txt";
  in do
    inputText   <- readFile inputPath
    let lns     = lines inputText
        (seedStr:_:mapLns) = lns
        seeds   = mkIntsFromLn seedStr
        mps     = mkDictsFromLns mapLns []
        rmps    = map dstToSrc (reverse mps)

        (one:_) = mps
    print $ spanSrcRgs (l, l+width) one
        where (l:width:_) = seeds
    --putStrLn $ foldl (\ acm m -> concat [acm, (show $ Map.toAscList m), "\n\n"]) "" mps -- print each map as sorted list of items
    --print $ findDest 1 one
    --print $ minimum $ map (flip pipeSrcToDest mps) [1..100000]
    --print one


