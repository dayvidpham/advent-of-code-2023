module Scratch where

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

mapTuple3 :: (a -> b) -> (a, a, a) -> (b, b, b)
mapTuple3 f (x, y, z) = (f x, f y, f z)


lineToParts :: (String, String, String) -> [Integer] -> IO [Integer] 
lineToParts 
  (_, "", _) 
  parts = 
    return $ reverse parts


lineToParts
  (tops, curs, btms)
  parts = do
    --print "before:"
    --print tops
    --print curs
    --print btms
    --putStrLn $ concat ["dropN: ", show dropN, " takeN: ", show takeN]
    --print checkTops
    --print $ concat [drop dropN toss, numStr, take 1 rem]
    --print checkBtms
    --print isPart
    --putStrLn "==========="
    --print "after:"
    --print topss
    --print rem
    --print btmss
    --putStrLn ""

    case isPart of
        True  -> lineToParts next $ [read numStr :: Integer] ++ parts
        False -> lineToParts next $ parts
    where (toss, numRaw) = break isNumber curs
          (numStr, rem)  = span isNumber numRaw

          pre = hasSymbol $ drop dropN toss
          post = case rem of
            ""      -> False
            (x:_)   -> isSymbol x
          -- For debugging
          -- postChar = case rem of
          --   ""      -> ""
          --   (x:_)   -> (x:"")

          dropN = length toss - 1
          --takeN = if dropN > 0 then length numStr + 2 else length numStr + 1
          takeN = length numStr + 2
          (checkTops, remTops) = (splitAt takeN . drop dropN) tops
          (checkBtms, remBtms) = (splitAt takeN . drop dropN) btms
          isPart = pre || post || hasSymbol checkTops || hasSymbol checkBtms

          dropNextN = takeN-1
          topss = concat [drop dropNextN checkTops, remTops]
          btmss = concat [drop dropNextN checkBtms, remBtms]

          next = (topss, rem, btmss)

processLines :: String -> [String] -> Integer -> IO ()
processLines _ (ln:[]) acm = print acm
processLines above (ln:below:lns) acm = do
    parts <- lineToParts (above, ln, below) []
    let acmNext = acm + sum parts
    putStrLn $ (show acm) ++ "\t" ++ (show $ sum parts) ++ "\t" ++ (show parts)
    processLines ln (below:lns) acmNext

padLines :: [String] -> [String]
padLines []         = []
padLines lns@(ln:_) = concat [pad, padBody lns, pad]
  where pad = [replicate (length ln + 2) '.']
        padBody (ln:lns) = case (ln, lns) of
            (ln, [])  -> ["." ++ ln ++ "."]
            (ln, lns) -> ["." ++ ln ++ "."] ++ (padBody lns)

main :: IO ()
main =
    let
        inputPath   = "../3-input.txt";
    in do
        inputText       <- readFile inputPath
        let inLines     = lines inputText
        let (pad:lns)   = padLines inLines
        --putStrLn . show $ lns
        processLines pad lns 0
