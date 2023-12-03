data TrieNode = TrieNode {
    dict :: [(Char, TrieNode)]
} deriving (Eq, Show)
    
addWord :: String -> TrieNode -> TrieNode
addWord str root
    | null str = 
        root
    | null found =
        TrieNode $ (key, addWord suffix (TrieNode [])) : (dict root) 
    | otherwise =
        let
            (sx, post) = break ((key ==) . fst) (dict root)
            (x, xs) = if length post > 1
                then (head post, tail post)
                else (head post, [])

            rec = addWord suffix (snd x)
            y = (key, rec)
        in TrieNode $ concat [sx, [y], xs]
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

main :: IO ()
main =
    let
        a = addWord "hello" (TrieNode [])
        b = addWord "heart" a
    in putStrLn $ show $ b

-- main :: IO ()
-- main = 
--     putStrLn $ show $ nest "hello"
