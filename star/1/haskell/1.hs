import System.IO 
import Data.Typeable

isNumber' :: Char -> [Char] -> Bool
isNumber' c nums
    | [] == nums = False
    | c == head nums = True
    | otherwise = isNumber' c (tail nums)

isNumber :: Char -> Bool
isNumber c = isNumber' c ['0', '1', '2', '3', '4', '5', '6', '7', '8', '9']

charToInt :: Char -> Integer
charToInt x = case x of
    '1' -> 1
    '2' -> 2
    '3' -> 3
    '4' -> 4
    '5' -> 5
    '6' -> 6
    '7' -> 7
    '8' -> 8
    '9' -> 9
    '0' -> 0
    _ -> -1

lineToIntList' :: String -> [Integer] -> [Integer]
lineToIntList' str xs
    | null str = xs
    | conv /= -1 = lineToIntList' (tail str) (xs ++ [conv])
    | otherwise = lineToIntList' (tail str) xs
    where conv = charToInt (head str)

lineToIntList :: String -> [Integer]
lineToIntList str = lineToIntList' str []

lineToCalibration :: String -> IO Integer
lineToCalibration str =
    let 
        xs = lineToIntList str
        (x, y) = (head xs, last xs)
    in 
        return (x*10 + y)

printAllLines :: Integer -> Handle -> IO (Integer, Handle)
printAllLines accum handle = do { 
    done <- hIsEOF handle; 
    if done == True 
    then 
        return (accum, handle);
    else do {
        line <- hGetLine handle;
        num <- lineToCalibration line;
         putStrLn (show (accum+num));
        printAllLines (accum+num) handle;
    }
}

main :: IO (Integer, Handle)
main = do
    withFile "../1-input.txt" ReadMode (\handle -> printAllLines 0 handle);
