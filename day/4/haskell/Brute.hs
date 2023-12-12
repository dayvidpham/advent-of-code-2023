module Brute where

import System.IO
import Control.Arrow ((***))
import Control.Monad (join)

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

data Game = Game {
    gameId      :: Int,
    gameWins    :: [Int],
    gameDraws   :: [Int]
} deriving (Ord, Eq, Show)

mkParts :: [String] -> [(String, [String], [String])]
mkParts lns = map (splitParts . mkFlatTup . mkNum_Parts . mkGame_Nums) lns
    where mkGame_Nums ln                    = break (\ c -> c == ':') ln
          mkNum_Parts (game, nums)          = (game, break (\ c -> c == '|') $ drop 2 nums)
          mkFlatTup (game, (wins, draws))   = (game, wins, drop 2 draws)
          splitParts (game, wins, draws)    = (game, words wins, words draws)
          readParts (game, wins, draws)     = (game, winNums, drawsNums)
            where winNums = read wins :: [Int]
                  drawsNums = read draws :: [Int]

{-
mkParts :: [String] -> [(String, [String], [String])]
mkParts lns = map (readParts . splitParts . mkFlatTup . mkNum_Parts . mkGame_Nums) lns
    where mkGame_Nums ln                    = break (\ c -> c == ':') ln
          mkNum_Parts (game, nums)          = (game, break (\ c -> c == '|') $ drop 2 nums)
          mkFlatTup (game, (wins, draws))   = (game, wins, drop 2 draws)
          splitParts (game, wins, draws)    = (game, words wins, words draws)
          readParts (game, wins, draws)     = (game, (read wins :: [Int]), (read draws :: [Int]))
-}

main :: IO ()
main =
    let
        inputPath   = "../4-input.txt";
    in do
        inputText   <- readFile inputPath
        let lns     = lines inputText
            parts   = mkParts lns
            (gameStr, winsStr, drawsStr) = unzip3 $ mkParts lns
            --(wins, draws) = (join (***)) (map words) (winsStr, drawsStr)
        putStrLn $ foldl (\ acm part -> ("\n" ++ show part ++ acm)) "" (reverse parts)
        --print draws

