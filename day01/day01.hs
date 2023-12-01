module Main where

import Data.List
import Data.List.Split
import Data.Char
import Debug.Trace



findNumber :: [Char] -> Int
findNumber s = (aux s) *10 + aux (reverse s)
    where 
        aux [] = 0
        aux (x:xs) = if isDigit x then read [x] else aux xs


findNumber2 :: [Char] -> Int
findNumber2 s = 
    let f1 = isPrefixOf
        f2 = isSuffixOf
    in
        (aux f1 True s) * 10 + (aux f2 False s)
    where 
        aux _ _ [] = -1
        aux f b s@(x:xs) | f "one" s = 1
              | f "two" s = 2
              | f "three" s = 3
              | f "four" s = 4
              | f "five" s = 5
              | f "six" s = 6
              | f "seven" s = 7
              | f "eight" s = 8
              | f "nine" s = 9
              | otherwise =  if isDigit c then read [c] else aux f b rest
              where c = if b then x else last s 
                    rest = if b then xs else init s

main :: IO ()
main = do 
    lines <- lines <$> readFile "input.txt"
    let part1 = sum $ map findNumber lines
    let part2 = sum $ map findNumber2 lines
    print $ "Part1: " ++ show part1
    print $ "Part2: " ++ show part2