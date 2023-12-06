module CubeGame where

import System.IO

splitStr :: String -> String -> [String]
splitStr str seps = case dropWhile isSep str of
                        "" -> []
                        s' -> w : splitStr s'' seps
                            where (w, s'') = break isSep s'
                        where isSep = (\c -> elem c seps)

printAllLines :: Handle -> IO ()
printAllLines fp = do 
    isEOF <- hIsEOF fp
    case isEOF of   True  -> return ()
                    False -> do
                        ln      <- hGetLine fp
                        unGame  <- return $ drop 5 ln
                        splits  <- return $ splitStr unGame ",;: "
                        pairs   <- return $ map words splits
                        putStrLn . show $ pairs
                        printAllLines fp

main :: IO ()
main =
    let
        inputPath = "../2-input.txt";
    in do
        withFile "../2-input.txt" ReadMode printAllLines

