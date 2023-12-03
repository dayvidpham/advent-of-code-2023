data TrieNode = TrieNode {
    dict :: [(Char, TrieNode)]
} deriving (Eq, Show)
    
addWord :: String -> TrieNode -> TrieNode -> TrieNode
addWord str curr root
    | null str = 
        root
    | null found =
        let 
            updated = TrieNode $ (key, TrieNode []) : (dict root) 
            next    = snd $ head (dict updated)
        in
            addWord suffix next updated
    | otherwise =
        addWord suffix (snd $ head found) root
    where
        (key : suffix) = str
        found = filter (\item -> (fst item) == key) (dict root)

data Node k = Node {
    val :: k,
    nxt :: Node k
} | ListEnd deriving (Eq, Show)

nest :: String -> (Char, Node Char)
nest str = case str of
    []          -> ('\n', ListEnd)
    [x]         -> (x, Node '\n' ListEnd)
    (x:x2:xs)   -> (x, Node x2 $ snd $ nest (x2:xs))

-- main :: IO ()
-- main = 
--     putStrLn $ show $ addWord "hel" (TrieNode []) (TrieNode [])

main :: IO ()
main = 
    putStrLn $ show $ nest "hello"
