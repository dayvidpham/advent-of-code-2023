module Brute where

import System.IO

isNumber :: Char -> Bool
isNumber c = c `elem` "0123456789"

isSymbol :: Char -> Bool
isSymbol c = not $ c `elem` "0123456789."

hasSymbol :: String -> Bool
hasSymbol ln = case dropWhile notSymbol ln of
    "" -> False
    _  -> True
    where notSymbol c = not $ isSymbol c

indexOf :: (Eq a) => a -> [a] -> Int -> Int
indexOf y xs idx = case xs of
    []      -> -1
    (x:xss) -> case y == x of
        True  -> idx
        False -> indexOf y xss (idx+1)

----------------------------------------------------------
-- Brutish: no use of standard lib or Data.Set



main :: IO ()
main =
    let
        inputPath   = "../4-input.txt";
    in do
        inputText   <- readFile inputPath
        let lns = lines inputText
        print lns
