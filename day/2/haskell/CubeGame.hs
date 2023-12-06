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

dGetItem :: Ord k => k -> Dict k v -> Maybe (k, v)
dGetItem key dict = case found of
    []      -> Nothing
    (kv:_)  -> Just kv
    where found = filter (\itm -> key == (fst itm)) $ dItems dict

dPut :: Ord k => k -> v -> Dict k v -> Dict k v
dPut key val dict = case entry of
    []  -> Dict $ [(key, val)] ++ dItems dict
    _   -> Dict $ concat [pre, [(key, val)], post]
    where (pre, entry, post) = dGetPivot key dict

splitStr :: String -> String -> [String]
splitStr str seps = case dropWhile isSep str of
                        "" -> []
                        s' -> w : splitStr s'' seps
                              where (w, s'') = break isSep s'
                        where isSep = (\c -> elem c seps)

printAllLines :: Handle -> IO ()
printAllLines fp = do 
    isEOF <- hIsEOF fp
    case isEOF of   True  -> return ()
                    False -> do
                        ln      <- hGetLine fp
                        splits  <- return $ splitStr ln ",;:"
                        pairs   <- return $ map words splits
                        (id:vkColours) <- return $ pairs
                        kvColours <- return $ map reverse vkColours
                        putStrLn . show $ kvColours
                        printAllLines fp

main :: IO ()
main =
    let
        inputPath = "../2-input.txt";
    in do
        withFile "../2-input.txt" ReadMode printAllLines

