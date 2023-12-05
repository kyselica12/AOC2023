module Main where

import Data.List.Split (splitOn, chunksOf)
import Data.List (find)
import qualified Data.Map as M

type Range = (Int, Int)

parseRange :: String -> [((Int, Int), (Int, Int))]
parseRange content =  map parseLine $ tail $ lines content
    where
        parseLine line = let [a,b,c] = (map read . words) line in ((b, b+c-1), (a, a+c-1))

parseSeeds :: String -> [Int]
parseSeeds content = map read $ tail $ words content

parseInput :: String -> ([Int], [[(Range, Range)]])
parseInput content =
    let [seeds, a,b,c,d,e,f,g] = splitOn "\n\n" content
        seeds' = parseSeeds seeds
        ranges = map parseRange [a,b,c,d,e,f,g]
    in (seeds', ranges)


intersectRange :: Range -> Range -> Range
intersectRange (a,b) (c,d) =
    let s = max a c
        e = min b d
    in if s <= e then (s, e) else (-1, -1)

subtractRange :: Range -> Range -> [Range]
subtractRange (a,b) (c,d) =
    let s = max a c
        e = min b d
        s1 = (a, s-1)
        s2 = (e+1, b)
    in ([s1 | a <= s-1]) ++ ([s2 | e+1 <= b] )

translateRange :: Range -> Range -> Range -> Range
translateRange (s1, s2) (f1, f2) (t1,t2) = (t1 + s1 - f1, t2 - (f2 - s2))

movePart1 :: [[((Int, Int), (Int, Int))]] -> Int -> Int
movePart1 (r:ranges) seed =
    let x = find (\((b,b'), (a,a')) -> b <= seed && seed <= b') r
    in case x of
        Just ((b,b'), (a,a')) -> let off = seed - b in movePart1 ranges (a+off)
        Nothing -> movePart1 ranges seed 
movePart1 [] seed = seed


movePart2 :: [[((Int, Int), (Int, Int))]] -> (Int, Int) ->   Int
movePart2 ranges@(r:rs) s@(seed, off)  =
    let x = find (\(s2, _) -> intersectRange s s2 /= (-1,-1)) r
    in case x of
        Just (s1@(b,b'), s2@(a,a')) -> let int = intersectRange s s1
                                           s' = translateRange int s1 s2
                                           y = map (movePart2 ranges) (subtractRange s s1)
                                        in minimum $ (movePart2 rs s'):y
        Nothing -> movePart2 rs s
movePart2 [] s = fst s


main :: IO ()
main = do
    content <- readFile "day05/input.txt"
    let (seeds, ranges) = parseInput content
    let seeds' = map (\[x,y] -> (x,x+y-1)) $ chunksOf 2 seeds
    let part1 = minimum $ map (movePart1 ranges) seeds
    let part2 = minimum $ map (movePart2 ranges) seeds'
    print $ "Part 1: " ++ show part1
    print $ "Part 2: " ++ show part2
    return ()