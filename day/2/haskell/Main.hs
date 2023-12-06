module Main where

import System.IO

printAllLines :: Handle -> IO ()
printAllLines fp = do 
    isEOF <- hIsEOF fp
    case isEOF of   True  -> return ()
                    False -> do
                        ln <- hGetLine fp
                        putStrLn ln
                        printAllLines fp

main :: IO ()
main =
    let
        inputPath = "../2-input.txt";
    in do
        --putStrLn . show $ root
        out <- withFile "../2-input.txt" ReadMode printAllLines
        return ()

