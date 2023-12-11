import System.IO

data Dict k v = Dict {
    dItems :: [(k, v)]
} deriving (Ord, Eq, Show)

dGetPivot :: Ord k => k -> Dict k v -> ( [(k, v)], [(k, v)], [(k, v)] )
dGetPivot key dict = case pivots of
    (pre, [])    -> (pre, [], [])
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

-------------------------------------------------------------------------------

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

indexOf :: (Eq a) => a -> [a] -> Int -> Int
indexOf y xs idx = case xs of
    []      -> -1
    (x:xss) -> case y == x of
        True  -> idx
        False -> indexOf y xss (idx+1)

lineToParts :: (String, String, String) -> (Int, Int) -> [Int] -> Dict (Int, Int) [Int] -> IO ([Int], Dict (Int, Int) [Int])
lineToParts 
  (_, "", _) 
  (row, col)
  parts
  dict = 
    return $ (reverse parts, dict)

lineToParts
  (tops, curs, btms)
  (row, col)
  parts
  dict = do
    case isPart of
        True  -> lineToParts nextFeedLn nextRowCol ([currPart] ++ parts) nextDict
        False -> lineToParts nextFeedLn nextRowCol parts nextDict
    where 
        (toss, numRaw) = break isNumber curs
        (numStr, rem)  = span isNumber numRaw

        dropN = length toss - 1
        takeN = length numStr + 2

        preChar = drop dropN toss
        pre = hasSymbol $ preChar
        postNum = case rem of
          ""      -> ""
          (x:_)   -> (x:"")
        post = hasSymbol postNum

        (checkTops, remTops) = (splitAt takeN . drop dropN) tops
        (checkBtms, remBtms) = (splitAt takeN . drop dropN) btms
        checkCurs = concat [preChar, numStr, postNum]
        
        startCheckCol = col + dropN
        endCheckCol   = startCheckCol + takeN
        topCheckIdx = indexOf '*' checkTops 1
        curCheckIdx = indexOf '*' checkCurs 1
        btmCheckIdx = indexOf '*' checkBtms 1
        checkIdx = mkIdx [(row-1, topCheckIdx), (row, curCheckIdx), (row+1,btmCheckIdx)]
            where mkIdx = foldl (\ acm (r,c) -> if c == -1 then acm else (r, startCheckCol+c)) (-1,-1) 

        found = case checkIdx of
            (-1,-1) -> False
            _       -> True
        nextDict = case found of
            False   -> dict
            True    -> case maybeKV of
                            Nothing -> dPut checkIdx [currPart] dict
                            Just kv -> dPut checkIdx (v ++ [currPart]) dict
                                where (k,v) = kv
                            where maybeKV = dGetItem checkIdx dict

        isPart = pre || post || hasSymbol checkTops || hasSymbol checkBtms
        currPart = read numStr :: Int

        dropNextN = takeN-1
        topss = concat [drop dropNextN checkTops, remTops]
        btmss = concat [drop dropNextN checkBtms, remBtms]

        nextFeedLn = (topss, rem, btmss)
        nextRowCol = (row, col + dropN+1 + dropNextN-1)
        (_, nextCol) = nextRowCol


processLines :: String -> [String] -> Int -> Int-> Dict (Int, Int) [Int] -> IO ()
processLines _ (ln:[]) row acm dict = do 
    print acm
    print total
    where gearItems = filter (\ itm -> (length . snd $ itm) == 2) $ dItems dict
          total = sum $ map (product . snd) gearItems
          --gearRatios = map product gears

processLines above (ln:below:lns) row acm dict = do
    (parts, nextDict) <- lineToParts (above, ln, below) (row, 0) [] dict
    let acmNext = acm + sum parts
    putStrLn $ (show row) ++ "\t" ++ (show acm) ++ "\t" ++ (show $ sum parts) ++ "\t" ++ (show parts)
    processLines ln (below:lns) (row+1) acmNext nextDict

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
        processLines pad lns 1 0 (Dict [])
