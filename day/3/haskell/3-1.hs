import System.IO

splitStr :: String -> String -> [String]
splitStr str seps = case dropWhile isSep str of
                        "" -> []
                        s' -> w : splitStr s'' seps
                              where (w, s'') = break isSep s'
                        where isSep = (\c -> elem c seps)

partitionByKeys :: Ord k => [k] -> [(k,v)] -> [ (k,[v]) ]
partitionByKeys keys items = foldl partition [] keys
    where filterItems key   = filter ((== key) . fst) items
          partition acc key = acc ++ [(key, map (snd) $ filterItems key)]

printAllLines :: Integer -> Handle -> IO Integer
printAllLines acc fp = do 
    isEOF <- hIsEOF fp
    case isEOF of   
        True  -> return acc
        False -> do
            ln <- hGetLine fp
            let splits              = splitStr ln ",;:"
            let (idList:colourList) = map words splits
            --let (idK:idVStr:_)      = idList 
            let (_:idVStr:_)          = idList 
            let idV                 = read idVStr :: Integer
            --let idItem              = (idK, idV)
            let colourItems         = map revListToItem colourList
            let dict                = Dict $ partitionByKeys clrs colourItems
            let maxCnts             = fmap (\clr -> clrToMaxCnt clr dict) clrs
            let power               = foldl (*) 1 maxCnts
            printAllLines (acc+power) fp
            where clrs = ["red", "green", "blue"]
                  revListToItem vk  = let (v:k:_) = vk in (k, read v :: Integer)
                  clrToMaxCnt clr ddict = case mns of
                    Nothing -> -1
                    Just ns -> foldl max 0 ns
                    where mns   = dGet clr ddict

main :: IO Integer
main =
    let
        inputPath = "../2-input.txt";
    in do
        withFile "../2-input.txt" ReadMode (printAllLines 0)

