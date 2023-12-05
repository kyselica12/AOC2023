module Main where

import Data.List.Split (splitOn, chunksOf)
import Data.List
import qualified Data.Map as M
import Debug.Trace (trace)

type Range = (Int, Int)

-- parseRange :: String -> M.Map Int Int
parseRange :: String -> [((Int, Int), (Int, Int))]
parseRange content =  map parseLine $ tail $ lines content
    where
        parseLine line = let [a,b,c] = (map read . words) line in ((b, b+c-1), (a, a+c-1))

parseSeeds :: String -> [Int]
parseSeeds content = map read $ tail $ words content

-- parseInput :: [Char] -> [Char]
parseInput content =
    let [seeds, a,b,c,d,e,f,g] = splitOn "\n\n" content
        seeds' = parseSeeds seeds
        ranges = map parseRange [a,b,c,d,e,f,g]
    in (seeds', ranges)

move :: Int -> [[((Int, Int), (Int, Int))]] -> Int
move seed (r:ranges) = 
    let x = find (\((b,b'), (a,a')) -> b <= seed && seed <= b') r
    in case x of
        Just ((b,b'), (a,a')) -> let off = seed - b in move (a+off) ranges
        Nothing -> move seed ranges
move seed [] = seed

intersectRange :: Range -> Range -> Range
intersectRange (a,b) (c,d) = 
    let s = max a c
        e = min b d
    in if s <= e then (s, e) else (-1, -1)

move2 :: (Int, Int) -> [[((Int, Int), (Int, Int))]] -> Int
move2 s@(seed, off) (r:ranges) = 
    let x = find (\(s2, _) -> intersectRange s s2 /= (-1,-1)) r
    in case x of
        Just (s1@(b,b'), s2@(a,a')) -> let (c, c') = intersectRange s s1 in 
                                       move2 s ranges
        Nothing -> move2 s ranges
    where 
        moveSubRange s1@(b,b') s2@(a,a') = 
            let (c, c') = intersectRange s s1 
                (d, d') = (a+c-b, a'+b'-c')
            in 
            if c == -1 then s2 else (a + c - b, a' + c' - b')
     


main :: IO ()
main = do
    content <- readFile "day05/input.txt"
    let (seeds, ranges) = parseInput content
    let seeds' = map (\[x,y] -> (x,x+y-1)) $ chunksOf 2 seeds
    let part1 = minimum $ map (\s -> move s ranges) seeds 
    let part2 = 0
    -- print $ map (\s -> move s ranges) seeds
    print $ "Part 1: " ++ show (part1)
    print $ "Part 2: " ++ show (part2)
    return ()