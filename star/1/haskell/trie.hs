import System.IO
import Data.Maybe


---------------------------------------------------
-- Linked List implementation:
--   * Learning experience
--   * Was a good prep for the Trie
data Node k = Node {
    val :: k,
    nxt :: Node k
} | ListEnd deriving (Eq, Show)

nest :: String -> (Char, Node Char)
nest str = case str of
    []          -> ('\n', ListEnd)
    [x]         -> (x, Node '\n' ListEnd)
    (x:x2:xs)   -> (x, Node x2 $ snd $ nest (x2:xs))


---------------------------------------------------
-- Trie implementation
data TrieNode = 
    TrieNode { dict :: [(Char, TrieNode)] } 
    | TrieEnd { valLen :: (Integer, Integer) }
    deriving (Eq, Show)


fstItemMatch :: Char -> TrieNode -> Maybe TrieNode
fstItemMatch key root = case pivot of
    []              -> Nothing
    ((_, next):_)   -> Just next
    where items = dict root
          pivot = dropWhile ((/= key) . fst) items
          

-- Can refactor to use fstItemMatch
getWord :: String -> TrieNode -> (Bool, Maybe TrieNode)
getWord str root
    | null str = 
        let end = filter ((== '\n') . fst) items
        in case end of
            ((_, x):_)  -> (True, Just x)
            otherwise   -> (False, Nothing)
    | otherwise = case matches of
        []        -> (False, Nothing)
        (match:_) -> getWord suffix (snd match)
    where
        (key:suffix) = str
        items = dict root
        matches = dropWhile ((/= key) . fst) items


strToInt :: String -> (Integer, Integer)
strToInt x = case x of
    "0"     -> (0, n)
    "1"     -> (1, n)
    "one"   -> (1, n)
    "2"     -> (2, n)
    "two"   -> (2, n)
    "3"     -> (3, n)
    "three" -> (3, n)
    "4"     -> (4, n)
    "four"  -> (4, n)
    "5"     -> (5, n)
    "five"  -> (5, n)
    "6"     -> (6, n)
    "six"   -> (6, n)
    "7"     -> (7, n)
    "seven" -> (7, n)
    "8"     -> (8, n)
    "eight" -> (8, n)
    "9"     -> (9, n)
    "nine"  -> (9, n)
    _       -> (-1, -1)
    where n = toInteger . length $ x


addWord' :: String -> String -> TrieNode -> TrieNode
addWord' str og root
    | null str = -- maybe redundant with new case below
        root
    | key == '\n' =
        TrieNode $ (key, TrieEnd . strToInt $ og) : items
    | null found =
        TrieNode $ (key, addWord' suffix og (TrieNode [])) : items
    | otherwise =
        let
            (sx, (x:xs)) = break ((key ==) . fst) items
            rec = addWord' suffix og (snd x)
            y = (key, rec)
        in TrieNode $ concat [sx, [y], xs]
    where
        (key : suffix) = str
        items = dict root
        found = filter (\item -> (fst item) == key) items


addWord :: String -> TrieNode -> TrieNode
addWord str root = addWord' (str ++ "\n") str root


---------------------------------------------------
-- Start of actual problem solving code
feedStrTrie :: String -> TrieNode -> (Integer, String)
feedStrTrie [] root = case end of
    Nothing -> (-1, "")
    Just a  -> (fst . valLen $ a, "") -- only path where TrieEnd can be returned
    where end = fstItemMatch '\n' root

feedStrTrie str@(x:xs) root = case end of
    Just value  -> (fst . valLen $ value, str) 
    Nothing     -> case next of
        Nothing   -> feedStrTrie xs root
        Just node -> feedStrTrie xs node
    where end  = fstItemMatch '\n' root
          next = fstItemMatch x root


lineToIntList :: String -> TrieNode -> [Integer] -> [Integer]
lineToIntList str root xs = case conv of
    -- | conv /= -1 = lineToIntList suffix root (xs ++ [conv])
    -- | otherwise  = lineToIntList suffix root xs
    -1 -> lineToIntList suffix root xs 
    _  -> lineToIntList suffix root ([conv] ++ xs)
    where (conv, suffix) = feedStrTrie str root

{-
lineToCalibration :: String -> IO Integer
lineToCalibration str =
    let 
        xs = lineToIntList str
        (x, y) = (head xs, last xs)
    in 
        return (x*10 + y)
-}

--hFeedStrTrie :: IO String 

--printAllLines :: Integer -> Handle -> IO (Integer, Handle)
printAllLines :: Integer -> TrieNode -> Handle -> IO ()
printAllLines accum root handle = do { 
    done <- hIsEOF handle; 
    if done == True 
    then 
        --return (accum, handle);
        return ();
    else do {
        line <- hGetLine handle;
        x <- return (feedStrTrie line root);
        putStrLn . show $ x;
        printAllLines (accum) root handle;
        --num <- lineToCalibration line;
        --putStrLn (show (accum+num));
        --printAllLines (accum+num) handle;
    }
}


--main :: IO (Integer, Handle)
main :: IO ()
main =
    let
        vocab = [ 
            "1", "2", "3", "4", "5", "6", "7", "8", "9", "0",
            "one", "two", "three", "four", "five", "six", "seven", "eight", "nine" ]
        root = foldr (\str -> \trie -> addWord str trie) (TrieNode []) vocab
    in do
        withFile "../1-input.txt" ReadMode (\handle -> printAllLines 0 root handle)
        putStrLn . show $ root


