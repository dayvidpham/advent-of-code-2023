module CubeGame where

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

printAllLines :: Integer -> Dict String Integer -> Handle -> IO ()
printAllLines acc maxs fp = do 
    isEOF <- hIsEOF fp
    case isEOF of   
        True  -> return ()
        False -> do
            ln <- hGetLine fp
            let splits              = splitStr ln ",;:"
            let (idList:colourList) = map words splits
            let (idK:idV:_)         = idList 
            let id                  = (idK, read idV :: Integer)
            let colourItems         = map revListToItem colourList
            let dict                = Dict $ partitionByKeys clrs colourItems
            let clrToReality        = fmap (\clr -> (clrInBounds clr dict maxs)) clrs
            putStrLn . show $ dict
            putStrLn . show $ reality
            printAllLines acc maxs fp
            where clrs = ["red", "green", "blue"]
                  revListToItem vk  = let (v:k:_) = vk in (k, read v :: Integer)
                  clrInBounds clr ddict dmaxs = case (mMaxN, mns) of
                    (Nothing, Nothing)      -> True
                    (Nothing, _)            -> False
                    (_, Nothing)            -> True
                    (Just maxN, Just ns)    -> and $ fmap (maxN >=) ns
                    where mMaxN = dGet clr dmaxs
                          mns   = dGet clr ddict

main :: IO ()
main =
    let
        inputPath = "../2-input.txt";
        maxs = Dict [("red", 12), ("green", 13), ("blue", 14)]
    in do
        withFile "../2-input.txt" ReadMode (printAllLines 0 maxs)

