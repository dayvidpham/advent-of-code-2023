module Scratch where

import System.IO
import Control.Arrow

data Dict k v = Dict {
    dItems :: [(k, v)]
} deriving (Ord, Eq, Show)

dGetPivot :: Ord k => k -> Dict k v -> ( [(k, v)], [(k, v)], [(k, v)] )
dGetPivot key dict = case pivots of
    (pre, pivot) -> (pre, [match], post)
        where 
            (match:post) = pivot
    where 
        isKey = (\x -> key == (fst x))
        pivots = break isKey $ dItems dict 

dPut :: Ord k => k -> v -> Dict k v -> Dict k v
dPut key val dict = case entry of
    []  -> Dict $ [(key, val)] ++ dItems dict
    _   -> Dict $ concat [pre, [(key, val)], post]
    where (pre, entry, post) = dGetPivot key dict

dGetItem :: Ord k => k -> Dict k v -> Maybe (k, v)
dGetItem key dict = case found of
    []      -> Nothing
    (kv:_)  -> Just kv
    where found = filter (\itm -> key == (fst itm)) $ dItems dict

dGet :: Ord k => k -> Dict k v -> Maybe v
dGet key dict = case found of
    Nothing  -> Nothing
    Just kv  -> Just $ snd kv
    where found = dGetItem key dict

splitStr :: String -> String -> [String]
splitStr str seps = case dropWhile isSep str of
                        "" -> []
                        s' -> w : splitStr s'' seps
                              where (w, s'') = break isSep s'
                        where isSep = (\c -> elem c seps)

partitionByKeys :: Ord k => [k] -> [(k,v)] -> [ (k,[v]) ]
partitionByKeys keys items = foldl partition [] keys
    where filterItems key   = filter ((== key) . fst) items
          partition acc key = acc ++ [(key, map (snd) $ filterItems key)]

printAllLines :: Integer -> Handle -> IO Integer
printAllLines acc fp = do 
    isEOF <- hIsEOF fp
    case isEOF of   
        True  -> return acc
        False -> do
            ln <- hGetLine fp
            let splits              = splitStr ln ",;:"
            let (idList:colourList) = map words splits
            let (_:idVStr:_)        = idList 
            let idV                 = read idVStr :: Integer
            let colourItems         = map revListToItem colourList
            let dict                = Dict $ partitionByKeys clrs colourItems
            let maxCnts             = fmap (\clr -> clrToMaxCnt clr dict) clrs
            let power               = foldl (*) 1 maxCnts
            printAllLines (acc+power) fp
            where clrs = ["red", "green", "blue"]
                  revListToItem vk  = let (v:k:_) = vk in (k, read v :: Integer)
                  clrToMaxCnt clr ddict = case mns of
                    Nothing -> -1
                    Just ns -> foldl max 0 ns
                    where mns   = dGet clr ddict

---------------------------------------------------------------------------------------------------------

{-
lineToDict :: (Integer -> Integer -> Char -> Dict) -> Integer -> String -> Dict
lineToDict fPutItem col ln = case dropWhile isDot ln of
    ""  -> ,
    where isDot c = c == '.'
-}

isNumber :: Char -> Bool
isNumber c = c `elem` "0123456789"

isSymbol :: Char -> Bool
isSymbol c = not $ c `elem` "0123456789."

hasSymbol :: String -> Bool
hasSymbol ln = case dropWhile notSymbol ln of
    "" -> False
    _  -> True
    where notSymbol c = not $ isSymbol c

mapTuple3 :: (a -> b) -> (a, a, a) -> (b, b, b)
mapTuple3 f (x, y, z) = (f x, f y, f z)


lineToParts :: (String, String, String) -> [Integer] -> [Integer] 
lineToParts 
  (_, "", _) 
  parts = 
    parts


lineToParts
  (tops, curs, btms)
  parts = do
    case isPart of
        True  -> lineToParts next $ [read numStr :: Integer] ++ parts
        False -> lineToParts next $ parts
    where (toss, numRaw) = break isNumber curs
          (numStr, rem)  = span isNumber numRaw

          pre = hasSymbol $ drop dropN toss
          post = case rem of
            ""      -> False
            (x:_)   -> isSymbol x
          -- For debugging
          -- postChar = case rem of
          --   ""      -> ""
          --   (x:_)   -> (x:"")

          dropN = length toss - 1
          takeN = if dropN > 0 then length numStr + 2 else length numStr + 1

          (checkTops, remTops) = (splitAt takeN . drop dropN) tops
          (checkBtms, remBtms) = (splitAt takeN . drop dropN) btms
          isPart = pre || post || hasSymbol checkTops || hasSymbol checkBtms

          dropNextN = case dropN of
            0 -> takeN
            _ -> takeN-1
          topss = concat [drop dropNextN checkTops, remTops]
          btmss = concat [drop dropNextN checkBtms, remBtms]

          next = (topss, rem, btmss)

processLines :: String -> [String] -> Integer -> IO ()
processLines _ (ln:[]) acm = print acm
processLines above (ln:below:lns) acm = do
    putStrLn $ (show acm) ++ "\t" ++ (show $ sum parts) ++ "\t" ++ (show parts)
    processLines ln (below:lns) acmNext
    where parts = lineToParts (above, ln, below) []
          acmNext = acm + sum parts

padLines :: [String] -> [String]
padLines []         = []
padLines lns@(ln:_) = concat [pad, padBody lns, pad]
  where pad = [replicate (length ln + 2) '.']
        padBody (ln:lns) = case (ln, lns) of
            (ln, [])  -> ["." ++ ln ++ "."]
            (ln, lns) -> ["." ++ ln ++ "."] ++ (padBody lns)

main :: IO ()
main =
    let
        inputPath   = "../3-input.txt";
    in do
        inputText       <- readFile inputPath
        let inLines     = lines inputText
        let (pad:lns)   = padLines inLines
        --putStrLn . show $ lns
        processLines pad lns 0

