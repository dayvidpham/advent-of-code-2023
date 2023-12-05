module Trie where

import System.IO
import Data.Maybe


---------------------------------------------------
-- Linked List implementation:
--   * Learning experience
--   * Was a good prep for the Trie

{-
data Node k = Node {
    val :: k,
    nxt :: Node k
} | ListEnd deriving (Eq, Show)

nest :: String -> (Char, Node Char)
nest str = case str of
    []          -> ('\n', ListEnd)
    [x]         -> (x, Node '\n' ListEnd)
    (x:x2:xs)   -> (x, Node x2 $ snd $ nest (x2:xs))
-}


---------------------------------------------------
-- Trie implementation
data TrieNode = 
    TrieNode { dict :: [(Char, TrieNode)] } 
    | TrieEnd { val :: Integer }
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


strToInt :: String -> Integer
strToInt x = case x of
    "0"     -> 0
    "1"     -> 1
    "one"   -> 1
    "2"     -> 2
    "two"   -> 2
    "3"     -> 3
    "three" -> 3
    "4"     -> 4
    "four"  -> 4
    "5"     -> 5
    "five"  -> 5
    "6"     -> 6
    "six"   -> 6
    "7"     -> 7
    "seven" -> 7
    "8"     -> 8
    "eight" -> 8
    "9"     -> 9
    "nine"  -> 9
    _       -> -1


addWord' :: String -> String -> TrieNode -> TrieNode
addWord' str og root
    | null str = -- maybe redundant with new case below
        root
    | key == '\n' =
        TrieNode $ (key, TrieEnd $ strToInt og) : items
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
feedStrTrie :: String -> TrieNode -> Integer
feedStrTrie [] root = case end of
    Nothing -> -1
    Just leaf  -> val leaf -- only path where TrieEnd can be returned
    where end = fstItemMatch '\n' root

feedStrTrie str@(x:xs) root = case end of
    Just leaf  -> val leaf
    Nothing     -> case next of
        Nothing   -> -1 -- no match: hit max depth on this path
        Just node -> feedStrTrie xs node
    where end  = fstItemMatch '\n' root
          next = fstItemMatch x root


lineToIntList :: String -> TrieNode -> [Integer] -> [Integer]
lineToIntList str root xs | null str  = reverse xs
                          | otherwise = case matchResult of
                              -1  -> lineToIntList cs root xs 
                              _   -> lineToIntList cs root ([matchResult] ++ xs)
                              where matchResult = feedStrTrie str root
                                    (_:cs)      = str


lineToCalibration :: String -> TrieNode -> Integer
lineToCalibration str root =
    let 
        xs = lineToIntList str root []
        (x, y) = (head xs, last xs)
    in 
        x*10 + y


printAllLines :: Integer -> TrieNode -> Handle -> IO (Integer, Handle)
printAllLines accum root handle = do { 
    done <- hIsEOF handle; 
    if done == True 
    then 
        return (accum, handle);
    else do {
        line <- hGetLine handle;
        xs <- return (lineToIntList line root []);
        x <- return (lineToCalibration line root);
        putStrLn . concat $ [show x, " ", show xs, " ", line];
        printAllLines (accum+x) root handle;
    }
}


main :: IO ()
main =
    let
        vocab = [ 
            "1", "2", "3", "4", "5", "6", "7", "8", "9", "0",
            "one", "two", "three", "four", "five", "six", "seven", "eight", "nine" ]
        root = foldr (\str -> \trie -> addWord str trie) (TrieNode []) vocab
    in do
        --putStrLn . show $ root
        (calibration, path) <- withFile "../1-input.txt" ReadMode (printAllLines 0 root)
        putStrLn $ show calibration
        putStrLn $ show path


