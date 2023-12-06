 module Main where

import Data.List.Split (splitOn)
import Data.List
import System.TimeIt

parseInput :: String -> [(Int, Int)]
parseInput  = map (\[x,y] -> (x,y)) . transpose . map (map read . tail.words) . lines

parseInput2 :: String -> (Int, Int)
parseInput2  = (\[x,y] -> (x,y)) .  map (read.concat) . map (tail.words) . lines

boatRace :: Int ->  Int -> Int
boatRace t d = length $ takeWhile (>d) $ dropWhile (<=d) $ map (\x-> x * (t-x)) [0..t]

binarySearch :: Int -> Int -> (Int -> Bool) -> Int
binarySearch low high goLeft
    | low == high = low
    | goLeft mid = binarySearch low mid goLeft
    | otherwise = binarySearch (mid+1) high goLeft
    where mid = (low + high) `div` 2

boatRaceBS :: Int -> Int -> Int
boatRaceBS t d = end - start
    where
        start = binarySearch 0 t (\x -> x*(t-x) > d)
        end = binarySearch start t (\x -> x*(t-x) <= d)

part1 :: [(Int, Int)] -> Int
part1 races = product $ map (uncurry boatRaceBS) races

-- 13.8s
-- drop from start -> takeWhile > d 
-- part2 = boatRace

-- 22.39 s
-- drop from start -> reverse -> drop 
-- part2 t d = length $ reverse $ dropWhile (<=d) $ map (\x-> x * (t-x)) [0..t] 

-- Instanteous :)
part2 :: Int -> Int -> Int
part2 = boatRaceBS

main :: IO ()
main = do
    input <- readFile "input.txt"
    let parsed = parseInput input
    let parsed2 = parseInput2 input
    print "Day 06"
    timeIt $ print $ "Part 1: " ++ show (part1 parsed)
    timeIt $ print $ "Part 2: " ++ show (uncurry part2 parsed2)

