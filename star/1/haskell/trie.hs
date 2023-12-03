data TrieNode = TrieNode {
    dict :: [(Char, TrieNode)]
} deriving (Eq, Show)
addWord' :: String -> TrieNode -> TrieNode
addWord' str root
    | null str = 
        root
    | null found =
        TrieNode $ (key, addWord' suffix (TrieNode [])) : (dict root) 
    | otherwise =
        let
            (sx, (x:xs)) = break ((key ==) . fst) (dict root)
            rec = addWord' suffix (snd x)
            y = (key, rec)
        in TrieNode $ concat [sx, [y], xs]
    where
        (key : suffix) = str
        found = filter (\item -> (fst item) == key) (dict root)


addWord :: String -> TrieNode -> TrieNode
addWord str root = addWord' (str ++ "\n") root


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
    in putStrLn $ show root

--main :: IO ()
--main =
--    let
--        a = addWord "hello" (TrieNode [])
--        b = addWord "heart" a
--    in putStrLn $ show $ b

-- main :: IO ()
-- main = 
--     putStrLn $ show $ nest "hello"
