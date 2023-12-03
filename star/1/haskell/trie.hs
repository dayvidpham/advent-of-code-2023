data TrieNode = 
    TrieNode { dict :: [(Char, TrieNode)] } 
    | TrieEnd (Integer, String)
    deriving (Eq, Show)


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
    _ -> -1

addWord' :: String -> String -> TrieNode -> TrieNode
addWord' str og root
    | null str = -- maybe redundant with new case below
        root
    | key == '\n' =
        TrieNode $ (key, TrieEnd (strToInt og, og)) : items
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


data Node k = Node {
    val :: k,
    nxt :: Node k
} | ListEnd deriving (Eq, Show)

nest :: String -> (Char, Node Char)
nest str = case str of
    []          -> ('\n', ListEnd)
    [x]         -> (x, Node '\n' ListEnd)
    (x:x2:xs)   -> (x, Node x2 $ snd $ nest (x2:xs))


main :: IO ()
main =
    let
        vocab = [ 
            "1", "2", "3", "4", "5", "6", "7", "8", "9", "0",
            "one", "two", "three", "four", "five", "six", "seven", "eight", "nine" ]
        root = foldr (\str -> \trie -> addWord str trie) (TrieNode []) vocab
    in do
        putStrLn $ show root
        putStrLn $ show $ getWord "one" root
        putStrLn $ show $ getWord "onus" root
        putStrLn $ show $ getWord "1" root


--main :: IO ()
--main =
--    let
--        a = addWord "hello" (TrieNode [])
--        b = addWord "heart" a
--    in putStrLn $ show $ b

-- main :: IO ()
-- main = 
--     putStrLn $ show $ nest "hello"
