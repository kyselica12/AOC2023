 module Main where

import Data.List.Split (splitOn)
import Data.List

parseInput :: String -> [(Int, Int)]
parseInput  = map (\[x,y] -> (x,y)) . transpose . map (map read . tail.words) . lines

parseInput2 :: String -> (Int, Int)
parseInput2  = (\[x,y] -> (x,y)) .  map (read.concat) . map (tail.words) . lines

boatRace :: Int ->  Int -> Int
boatRace t d = length $ takeWhile (>d) $ dropWhile (<=d) $ map (\x-> x * (t-x)) [0..t]

part1 :: [(Int, Int)] -> Int
part1 races = product $ map (uncurry boatRace) races

part2 = boatRace

main :: IO ()
main = do
    input <- readFile "input.txt"
    let parsed = parseInput input
    let parsed2 = parseInput2 input
    print "Day 06"
    print $ "Part 1: " ++ (show $ part1 parsed)
    print $ "Part 2: " ++ show (uncurry part2 parsed2)

