import System.IO 
import Data.Typeable

-- List experiments

explode :: String -> (IO (), String)
explode str =
    case str of
        "" -> (putStrLn "", "")
        otherwise -> (putStrLn str >> io, str)
        where (io, rec) = explode (tail str)

numberFromLine :: String -> Integer -> Integer -> Integer
numberFromLine str first last = last

printAllLines :: Integer -> Handle -> IO (Integer, Handle)
printAllLines accum handle = do { 
    done <- hIsEOF handle; 
    if done == True 
    then 
        return (accum, handle);
    else do 
        line <- hGetLine handle;
        fst (explode line);
        printAllLines accum handle;
}

main :: IO (Integer, Handle)
main = do
    withFile "../1-input.txt" ReadMode (\handle -> printAllLines 0 handle);
