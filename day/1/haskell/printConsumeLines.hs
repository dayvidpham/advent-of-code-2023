import System.IO 
import Data.Typeable

-- List experiments

printConsume :: String -> (IO (), String)
printConsume str =
    case str of
        "" -> (putStrLn "", "")
        otherwise -> (putStrLn str >> io, str)
        where (io, rec) = printConsume (tail str)

printAllLines :: Integer -> Handle -> IO (Integer, Handle)
printAllLines accum handle = do { 
    done <- hIsEOF handle; 
    if done == True 
    then 
        return (accum, handle);
    else do 
        line <- hGetLine handle;
        fst (printConsume line);
        printAllLines accum handle;
}

main :: IO (Integer, Handle)
main = do
    withFile "../1-input.txt" ReadMode (\handle -> printAllLines 0 handle);
