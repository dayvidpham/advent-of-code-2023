import Trie

vocab = [ 
    "1", "2", "3", "4", "5", "6", "7", "8", "9", "0",
    "one", "two", "three", "four", "five", "six", "seven", "eight", "nine" ]
root = foldr (\str -> \trie -> addWord str trie) (TrieNode []) vocab
