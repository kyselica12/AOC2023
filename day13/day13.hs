module Main where

import Data.List.Split (splitOn)
import qualified Data.Map as M
import Data.List
import Debug.Trace (trace)
import Data.Maybe (fromJust)

parseInput :: String -> [[String]]
parseInput input = map lines $ splitOn "\n\n" input



part1 :: [[String]] -> Int
part1 input = trace (show xxxx) sum xxxx
    where
        xxxx = map solveBlock input
        solveBlock block = 
            case trace (show (map (\i-> (i, aux i block)) [1.. length block-1])) find (\i-> aux i block) (reverse [1..length block-1]) of
            Just i -> 100 *i
            Nothing -> case (find (\i-> aux i (transpose block)) [1..length block-1]) of 
                Just j -> j
                Nothing -> 0
        aux :: Int -> [String] -> Bool
        aux i block =
            let d = min i (n-i)
                x = take d $ drop i block
                y = take d $ reverse (take i block)
                n = length block
            in  y == x


part2 = undefined


main :: IO ()
main = do
    input <- parseInput <$> readFile "day13/example.txt"
    print "Day 13"
    -- print input
    print $ head input
    print $ "Part 1: " ++ (show $ part1 input)
    -- print $ "Part 2: " ++ (show $ part2 parsed)

