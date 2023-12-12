module Main (main) where

import Data.List
import Data.List.Split (splitOn)
import Data.Map qualified as M
import System.TimeIt (timeIt)

parseInput :: String -> [(String, [Int])]
parseInput = map ((\[m, x] -> (m, map read (splitOn "," x))) . words) . lines

part1 = sum . map solveRow

part2 :: [(String, [Int])] -> Int
part2 = sum . map (solveRow . unfold)
  where
    unfold (ms, ns) = (intercalate "?" (replicate 5 ms), concat (replicate 5 ns))

solveRow :: (String, [Int]) -> Int
solveRow (input,ns) = mem M.! (ns, 0, input)
  where
    mem =
      M.fromList
        [ ((n, c, i), aux n c i)
          | n <- tails ns,
            c <- [0 .. maximum ns],
            i <- tails input
        ]
    aux [] 0 i = if notElem '#' i then 1 else 0
    aux [p] c [] = if c == p then 1 else 0
    aux (p : ps) c [] = 0
    aux (p : ps) 0 ('.' : is) = mem M.! (p : ps, 0, is)
    aux (p : ps) c ('.' : is) = if p == c then mem M.! (ps, 0, is) else 0
    aux (p : ps) c ('#' : is) = if p == c then 0 else mem M.! (p : ps, c + 1, is)
    aux (p : ps) c ('?' : is) = aux (p : ps) c ('.' : is) + aux (p : ps) c ('#' : is)
    aux n c i = error $ "impossible " ++ show (n, c, i)

main :: IO ()
main = do
  input <- readFile "day12/input.txt"
  let parsed = parseInput input
  print "Day 12"
  -- print $ parsed
  -- print $ uncurry solveRow $ last parsed
  timeIt $ print $ "Part 1: " ++ show (part1 parsed)
  -- print $ uncurry part2 $ head parsed
  timeIt $ print $ "Part 2: " ++ show (part2 parsed)
