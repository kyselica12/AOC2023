module Main where

import Data.List.Split (splitOn, split)
import qualified Data.Set as S

type Input = [Int]

parseLine :: String -> Int
parseLine line =
    let [_,cards] = splitOn ":" line
        [s1, s2] = map (S.fromList.words) (splitOn "|" cards)
    in S.size (S.intersection s1 s2)

parseInput :: [String] -> Input
parseInput = map parseLine

part1 :: Input -> Int
part1 = sum . map ((2^).subtract 1).filter (>0)

part2 :: Input -> Int
part2 = solve (repeat 1)
    where
        solve (c:cs) (x:xs) =
            let cs' = map (+c) (take x cs)  ++ drop x cs
            in c + solve cs' xs
        solve _ [] = 0

main :: IO ()
main = do
    lines <- lines <$> readFile "input.txt"
    let input = map parseLine lines
    print $ "Part 1: " ++ show (part1 input)
    print $ "Part 2: " ++ show (part2 input)