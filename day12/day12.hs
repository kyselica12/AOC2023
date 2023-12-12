module Main where

import Data.List.Split (splitOn)
import Data.List
import Debug.Trace (trace)

parseInput :: String -> [(String,[Int])]
parseInput = map ((\[m,x] -> (m,map read (splitOn "," x))) . words) . lines

solveRow' = solveRow []

solveRow :: String -> [Int] -> Int
solveRow (m:ms) (x:xs)
    | m == '.' = solveRow  ms (x:xs)
    | m == '#' = let (n, rest) = splitAt (x-1) ms
                     ms' = if null rest then [] else tail rest
                 in if notElem '.' n && length n == x-1 && ( null rest || head rest /= '#')
                 then  solveRow  ms' xs else 0
    | m == '?' = solveRow ('#':ms) (x:xs) + solveRow ('.':ms) (x:xs)
solveRow [] [] =  1
solveRow [] x =  0
solveRow x []=  if '#' `notElem` x then 1 else 0

part1 = sum .  map (uncurry solveRow )

part2 input = c
    where
        (ms,ns) = head input
        a = takeWhile ((>0).fst) $ map (\x -> (solveRow x ns, x)) (tails ms)
        c = takeWhile ((>0).fst) $ reverse $ map (\x -> (solveRow x ns, x)) (inits ms)
        minlen = length $ snd $ last a
        b =takeWhile ((>0).fst) $ map (\x -> (solveRow x ns, x)) (map (++ms) (inits (ms++ms++ms++ms)))





main :: IO ()
main = do
    input <- readFile "day12/input.txt"
    let parsed = parseInput input
    print "Day 12"
    -- print $ parsed
    -- print $ uncurry solveRow $ last parsed
    -- print $ "Part 1: " ++ show (part1 parsed)
    print $ "Part 2: " ++ (show $ part2 parsed)

