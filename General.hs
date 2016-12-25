module General where

import System.IO

hgetVersion = "0.0.1"

greedyTakeWhile :: (a -> Bool) -> [a] -> [a]
greedyTakeWhile func list = if func (last list) 
                                then init list
                                else greedyTakeWhile func (init list)

greedyBreak :: (a -> Bool) -> [a] -> ([a], [a])
greedyBreak test list = let (a, b) = break test (reverse list)
                        in (reverse b, reverse a)

map2 :: (a -> b -> c) -> [a] -> [b] -> [c]
map2 func l1 l2 = doMap func l1 l2 []
    where
        doMap _    []     _  keep = keep 
        doMap func (x:xs) x2 keep = doMap func xs x2 (keep ++ (map (func x) x2))


mapFile :: Handle -> (String -> b) -> IO [b]
mapFile file func = doLines func file [] >>= return . reverse
    where 
        doLines func file keep = do
            eof <- hIsEOF file
            case eof of
                True -> return keep
                False -> do
                    line <- hGetLine file
                    doLines func file ((func line) : keep)
            

