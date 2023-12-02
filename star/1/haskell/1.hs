import System.IO 
import Control.Monad

printAllLines :: Handle -> IO Handle
printAllLines handle = do
    done <- hIsEOF handle
    if done == True 
        then return handle 
        else do
            lines <- hGetLine handle 
            putStrLn lines
            printAllLines handle
    -- if done 
    --     then handle
    --     else
    --         putStrLn line
    --         printAllLines handle;

-- main = do
--     let filePath = "../1-input.txt";
--     withFile filePath ReadMode (\handle -> do
--         line <- hGetLine handle
--         putStrLn line)
    
main :: IO Handle
main = do
    let filePath = "../1-input.txt";
    withFile filePath ReadMode printAllLines
