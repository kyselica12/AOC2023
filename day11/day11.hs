module Main where 

import Data.List.Split (splitOn)
import Data.List 
import GHC.Parser.Lexer (xset)
import Debug.Trace (trace)

-- parseInput :: String -> [(Int, Int)]
parseInput content = [(i,j) | (i,xs) <- zip [0..] (lines content), (j,x) <- zip [0..] xs, x == '#' ]

-- part1 :: ([(Int, Int)], Int, Int) -> Int
part1 stars =  dist expandedCols `div` 2
    where
        sortStarsRows = sortOn fst stars
        expandRows e ((x1,x2):y@(y1,y2):xs) = 
            if y1 - x1 <= 1 then (x1+e,x2):expandRows e (y:xs) else (x1+e,x2):expandRows ((e+(y1-x1-1))) (y:xs) 
        expandRows e [(x1,x2)] = [(x1+e,x2)]
        expandedRows = expandRows 0 sortStarsRows
        expandCols e (x@(x1,x2):y@(y1,y2):xs) = 
            if y2 - x2 <= 1 then (x1,x2+e):expandCols e (y:xs) else (x1,x2+e):expandCols ((e+y2-x2-1)) (y:xs) 
        expandCols e [(x1,x2)] = [(x1,x2+e)]
        expandedCols = expandCols 0 $ sortOn snd expandedRows
        dist xs = sum [ abs(y1-x1) + abs(y2-x2) | (x1,x2) <- xs, (y1,y2) <- xs] 

part2 stars' =  dist expandedCols `div` 2
    where
        stars = map (\(x,y) -> (toInteger x, toInteger y)) stars'
        sortStarsRows = sortOn fst stars
        expandRows e ((x1,x2):y@(y1,y2):xs) = 
            if y1 - x1 <= 1 then (x1+e,x2):expandRows e (y:xs) else (x1+e,x2):expandRows ((e+(y1-x1-1)*(1000000-1))) (y:xs) 
        expandRows e [(x1,x2)] = [(x1+e,x2)]
        expandedRows = expandRows 0 sortStarsRows
        expandCols e (x@(x1,x2):y@(y1,y2):xs) = 
            if y2 - x2 <= 1 then (x1,x2+e):expandCols e (y:xs) else (x1,x2+e):expandCols (e+(y2-x2-1)*(1000000 - 1)) (y:xs) 
        expandCols e [(x1,x2)] = [(x1,x2+e)]
        expandedCols = expandCols 0 $ sortOn snd expandedRows
        dist xs = sum [ abs(y1-x1) + abs(y2-x2) | (x1,x2) <- xs, (y1,y2) <- xs] 

main :: IO ()
main = do
    input <- readFile "day11/input.txt"
    let parsed = parseInput input
    print "Day 11"
    -- print parsed
    print $ "Part 1: " ++ (show $ part1 parsed)
    print $ "Part 2: " ++ (show $ part2 parsed)

