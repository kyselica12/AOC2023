module Main where

import Data.List
import Data.List.Split
import Data.Char
import Debug.Trace



findNumber :: [Char] -> Int
findNumber s = head nums *10 + last nums
    where
        nums = map digitToInt $ filter isDigit s

stringDigits :: [[Char]]
stringDigits = ["one", "two", "three", "four", "five", "six", "seven", "eight", "nine"]


findNumber2 :: [Char] -> Int
findNumber2 s = head nums * 10 + last nums
    where
        nums = aux s
        aux [] = []
        aux s@(x:xs) = case findIndex (`isPrefixOf` s) stringDigits of
            Just i -> (i+1):aux xs
            Nothing -> if isDigit x then digitToInt x : aux xs else aux xs

main :: IO ()
main = do
    lines <- lines <$> readFile "input.txt"
    let part1 = sum $ map findNumber lines
    let part2 = sum $ map findNumber2 lines
    print $ "Part1: " ++ show part1
    print $ "Part2: " ++ show part2