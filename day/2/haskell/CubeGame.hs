module CubeGame where

import System.IO

splitStr :: String -> String -> [String]
splitStr str sep = [str]

printAllLines :: Handle -> IO ()
printAllLines fp = do 
    isEOF <- hIsEOF fp
    case isEOF of   True  -> return ()
                    False -> do
                        ln <- hGetLine fp
                        putStrLn . show $ splitStr ln ""
                        printAllLines fp

main :: IO ()
main =
    let
        inputPath = "../2-input.txt";
    in do
        withFile "../2-input.txt" ReadMode printAllLines

