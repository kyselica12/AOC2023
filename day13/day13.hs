module Main where

import Data.List.Split (splitOn)
import Data.List
import Debug.Trace (trace)
import Data.Maybe (fromJust)
import Control.Applicative 

parseInput :: String -> [[String]]
parseInput input = map lines $ splitOn "\n\n" input

equal (a:as,b:bs) = (a == b) && equal (as,bs)
equal _ = True

difference1 (a:as,b:bs) = case foldl (\acc (x,y) -> if x/=y then acc+1 else acc) 0 (zip a b) of
                            0 -> difference1 (as,bs)
                            1 -> equal (as,bs)
                            _ -> False
difference1 _ = False

part1 :: [[String]] -> Int
part1 = sum . map (solveBlock aux)
    where
        aux i block = let (a,b) = splitAt i block in  equal (reverse a, b)

solveBlock :: (Int -> [String] -> Bool) -> [String] -> Int
solveBlock f block = fromJust $ col <|> ((*100) <$> row) 
    where 
        row = find (`f` block) [1..length block-1]
        col = find (`f` transpose block) [1..length (head block)-1]

part2 :: [[String]] -> Int
part2 = sum . map (solveBlock aux)
    where
        aux i block = let (a,b) = splitAt i block in difference1 (reverse a, b) && not (equal (reverse a, b))

main :: IO ()
main = do
    input <- parseInput <$> readFile "day13/input.txt"
    print "Day 13"
    print $ "Part 1: " ++ show (part1 input)
    print $ "Part 2: " ++ show (part2 input)

