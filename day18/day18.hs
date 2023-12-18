module Main where

import Data.List.Split (splitOn)
import Data.List
import Debug.Trace (trace)

data Dir = U | D | L | R deriving (Show, Eq, Ord)

parseInput :: String -> [(Dir, Int, String)]
parseInput = map ((\[d,n,c] -> (toDir d, read n :: Int, c)).words) . lines
    where
        toDir d = case d of
            "U" -> U
            "D" -> D
            "L" -> L
            "R" -> R

borderPoints (x,y) ((d,n,_):inst) = case d of
    U -> (x,y) : borderPoints (x-n, y) inst
    D -> (x,y) : borderPoints (x+n, y) inst
    L -> (x,y) : borderPoints (x, y-n) inst
    R -> (x,y) : borderPoints (x, y+n) inst
borderPoints (x,y) [] = [(x,y)]

picksTheorem :: [(Int, Int)] -> Int
picksTheorem path = area + borderLen `div` 2 + 1
    where
        area = (`div` 2) $ sum $ zipWith (\(x1,y1) (x2,y2) -> x1*y2 - x2*y1) path (tail path ++ [head path])
        borderLen = sum $ zipWith (\(x1,y1) (x2,y2) -> abs (x1-x2) + abs (y1-y2)) path (tail path)

part1 :: [(Dir, Int, String)] -> Int
part1 input = picksTheorem (reverse points)
    where
        points = borderPoints (0,0) input

part2 :: [(a, b, [Char])] -> Int
part2 input = picksTheorem (reverse (borderPoints (0,0) (parse2 input)))
    where
        parse2 ((_,_,c):rest) =
            let c' = drop 1 (init c)
                hex = "0x" ++ drop 1 (init c') in (traslateDir (last c'),read hex :: Int, -1) : parse2 rest
        parse2 [] = []
        traslateDir c = case c of
            '0' -> R
            '1' -> D
            '2' -> L
            '3' -> U

main :: IO ()
main = do
    input <- parseInput <$> readFile "day18/input.txt"
    print "Day 18"
    print $ "Part 1: " ++ show (part1 input)
    print $ "Part 2: " ++ show (part2 input)

