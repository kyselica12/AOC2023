module Main (main) where

import Data.List
import Data.List.Split (splitOn)
import Data.Map qualified as M
import System.TimeIt (timeIt)

parseInput :: String -> [(String, [Int])]
parseInput = map ((\[m, x] -> (m, map read (splitOn "," x))) . words) . lines

part1 :: [(String, [Int])] -> Int
part1 = sum . map solveRow

part2 :: [(String, [Int])] -> Int
part2 = sum . map (solveRow . unfold)
    where unfold (ms, ns) = (intercalate "?" (replicate 5 ms), concat (replicate 5 ns))

solveRow :: (String, [Int]) -> Int
solveRow (ms, ns) = mem M.! (0, 0, 0)
    where
        ln = length ns
        lm = length ms
        mem = M.fromList [
            ((m,n,a), aux m n a c p) |
            (m,c) <- zip [0..] (ms ++ "X"),
            (n,p) <- zip [0..] (ns ++ [0]),
            a <- [0..maximum ns] ]
        aux m n a c p
            | n == ln && m == lm && a == 0 = 1
            | n == ln-1 && a == p && m == lm = 1
            | n == ln && a == 0 && c /= '#' = mem M.! (m+1, n, 0)
            | c == '.' && a == 0 = mem M.! (m+1, n, 0)
            | c == '.' && a == p = mem M.! (m+1, n+1, 0)
            | c == '#' && a < p = mem M.! (m+1, n, a+1)
            | c == '?' && a == 0 = mem M.! (m+1, n, 0) + mem M.! (m+1, n, 1)
            | c == '?' && a == p = mem M.! (m+1, n+1, 0)
            | c == '?' && a < p = mem M.! (m+1, n, a+1)
            | otherwise = 0

main :: IO ()
main = do
  parsed <- parseInput <$> readFile "day12/input.txt"
  print "Day 12"
  timeIt $ print $ "Part 1: " ++ show (part1 parsed)
  timeIt $ print $ "Part 2: " ++ show (part2 parsed)
