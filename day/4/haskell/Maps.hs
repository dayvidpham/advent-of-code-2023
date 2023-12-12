module Maps where

import System.IO
import Data.Set (Set)
import qualified Data.Set as Set

import Data.Map (Map)
import qualified Data.Map as Map

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

gameToTuple :: Game -> (Int, [Int], [Int])
gameToTuple gm = (id, wins, draws)
    where id = gameId gm
          wins = gameWins gm
          draws = gameDraws gm

mkGame_Nums :: String -> (String, String)
mkGame_Nums ln                  = break (\ c -> c == ':') ln

mkNum_Parts :: (String, String) -> (String, (String, String))
mkNum_Parts (game, nums)        = (gamePart, break (\ c -> c == '|') $ drop 2 nums)
    where gamePart = words game !! 1

mkFlatTup :: (String, (String, String)) -> (String, String, String)
mkFlatTup (game, (wins, draws)) = (game, wins, drop 2 draws)

splitParts :: (String, String, String) -> (String, [String], [String])
splitParts (game, wins, draws)  = (game, words wins, words draws)

mkGame :: (String, [String], [String]) -> Game
mkGame (game, wins, draws)      = Game gameNum winNums drawsNums
    where gameNum = read game :: Int
          winNums = map read wins :: [Int]
          drawsNums = map read draws :: [Int]

mkGamesFromLns :: [String] -> [Game]
mkGamesFromLns lns = map (mkGame . splitParts . mkFlatTup . mkNum_Parts . mkGame_Nums) lns

gameToPoints :: Game -> Int
gameToPoints gm = countWins (gameDraws gm) winsSet
    where winsSet = Set.fromList $ gameWins gm
          countWins draws set = foldl (\ acm draw -> if Set.member draw set then (acm+1) else acm) 0 draws

mkCopies :: Map Int Int -> (Int, Int) -> Map Int Int
mkCopies mp (i, points) = foldl addCopies mp [begin..end]
    where currCopies = case Map.lookup i mp of
                            Nothing -> 0
                            Just a  -> a
          addCopies mp idx = Map.insertWith (+) idx currCopies mp
          begin = i+1
          end = i+points


main :: IO ()
main =
    let
        inputPath   = "../4-input.txt";
    in do
        inputText   <- readFile inputPath
        let lns     = lines inputText
            games   = mkGamesFromLns lns
            points  = map gameToPoints games
            n       = length points
            (gms, wins, draws) = unzip3 $ map gameToTuple $ mkGamesFromLns lns
            init    = Map.fromAscList $ zip gms $ replicate n 1
            copies  = foldl mkCopies init $ zip gms points
            total   = sum $ Map.elems copies
            --(wins, draws) = (join (***)) (map words) (winsStr, drawsStr)
        --putStrLn $ foldl (\ acm part -> ("\n" ++ show part ++ acm)) "" (reverse games)
        print points
        print init
        print copies
        print $ total
        --print draws

