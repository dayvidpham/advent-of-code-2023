module Scratch where

import System.IO

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


processLines :: String -> [String] -> IO ()
processLines _ (ln:[]) = return ()
processLines above (ln:below:lns) = do
    putStrLn ln
    processLines ln (below:lns)

padLines :: [String] -> [String]
padLines []         = []
padLines lns@(ln:_) = concat [pad, lns, pad]
    where pad = [replicate (length ln) '.']

main :: IO ()
main =
    let
        inputPath   = "../3-input.txt";
    in do
        inputText   <- readFile inputPath
        let inLines = lines inputText
        let (pad:lns) = padLines inLines
        --putStrLn . show $ lns
        processLines pad lns

