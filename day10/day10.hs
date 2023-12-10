module Main where

import Data.List.Split (splitOn)
import Data.List
import Text.Parsec
import Data.Array (array, Array, (!), bounds, indices, listArray, assocs)
import qualified Data.Set as S
import Data.Maybe (catMaybes)
import GHC.Data.StringBuffer (StringBuffer(len))
import Debug.Trace (trace)
import System.TimeIt

type Pos = (Int, Int)

parseInput :: String -> (Pos, Array Pos Char)
parseInput input = (start, arr)
    where rows = lines input
          nr = length rows
          nc = length $ head rows
          arr = listArray ((0,0), (nr-1, nc-1)) $ concat rows
          start = fst . head . filter ((=='S').snd) $ assocs arr

move :: Array Pos Char -> Pos -> Pos -> Maybe Pos
move arr from@(f1, f2) p@(p1, p2) =
    let (i',j') = case arr ! p of
            '|' -> case from of
                (f1', f2') | f2' == p2 && f1'-1==p1 -> (p1-1, p2)
                (f1', f2') | f2' == p2 && f1'+1==p1 -> (p1+1, p2)
                _ -> (-1,-1)
            '-' -> case from of
                (f1', f2') | f1' == p1 && f2'-1==p2 -> (p1, p2-1)
                (f1', f2') | f1' == p1 && f2'+1==p2 -> (p1, p2+1)
                _ -> (-1,-1)
            'L' -> case from of
                (f1', f2') | f2' == p2 && f1'+1==p1 -> (p1, p2+1)
                (f1', f2') | f1' == p1 && f2'-1==p2 -> (p1-1, p2)
                _ -> (-1,-1)
            'J' -> case from of
                (f1', f2') | f2' == p2 && f1'+1==p1 -> (p1, p2-1)
                (f1', f2') | f1' == p1 && f2'+1==p2 -> (p1-1, p2)
                _ -> (-1,-1)
            '7' -> case from of
                (f1', f2') | f2' == p2 && f1'-1==p1 -> (p1, p2-1)
                (f1', f2') | f1' == p1 && f2'+1==p2 -> (p1+1, p2)
                _ -> (-1,-1)
            'F' -> case from of
                (f1', f2') | f2' == p2 && f1'-1==p1 -> (p1, p2+1)
                (f1', f2') | f1' == p1 && f2'-1==p2 -> (p1+1, p2)
                _ -> (-1,-1)
            '.' -> (-1,-1)
            _ -> error "Invalid move"
        ((minI, minJ), (maxI, maxJ)) = bounds arr
        in if minI <= i' && i' <= maxI && minJ <= j' && j' <= maxJ then Just (i', j') else Nothing


part1 :: (Pos, Array Pos Char) -> Int
part1 (s@(s1, s2), arr) =
    let (maxI, maxJ) = snd $ bounds arr
        starts = [(s1+i, s2+j) | (i,j) <- [(-1,0),(1,0), (0,1), (0,-1)], s1+i >= 0, s2+j >= 0, s1+i <= maxI, s2+j <= maxJ]
    in (`div` 2) . length . head . catMaybes $ map (loop arr [s]) starts

loop :: Array Pos Char -> [Pos] -> Pos -> Maybe [Pos]
loop arr path@(prev:_) curr =
    if curr == last path then Just (curr:path)
    else do
        x <- move arr prev curr
        loop arr (curr: path) x

expandPath :: [Pos] -> [Pos]
expandPath ((x1,x2):(y1,y2):rest) = (x1*2,x2*2):(x1*2+y1-x1, x2*2+y2-x2):expandPath ((y1,y2):rest)
expandPath [(x1,x2)] = [(x1*2,x2*2)]

-- part2 :: (Pos, Array Pos Char) -> Int
part2 :: ((Int, Int), Array (Int, Int) Char) -> Int
part2 (s@(s1, s2), arr) =
    S.size . S.filter (\(i,j) -> even i && even j) . S.difference allPosSet $ (S.union loopPos outBlobs)
    where
        (maxI, maxJ) = snd $ bounds arr
        starts = [(s1+i, s2+j) | (i,j) <- [(-1,0),(1,0), (0,1), (0,-1)], s1+i >= 0, s2+j >= 0, s1+i <= maxI, s2+j <= maxJ]
        allPosSet = S.fromList [(i,j) | i <- [0..maxI*2], j <- [0..maxJ*2]]
        loopPos = S.fromList . expandPath . head . catMaybes . map (loop arr [s]) $ starts
        borderPos = [(i,0) | i <- [0..maxI*2]] ++ [(i,maxJ*2) | i <- [0..maxI*2]] ++ [(0,j) | j <- [0..maxJ*2]] ++ [(maxI*2,j) | j <- [0..maxJ]]
        outBlobs = foldl (\a x -> S.union a (exploreBlob arr loopPos a x)) S.empty borderPos


exploreBlob :: Array Pos Char -> S.Set Pos -> S.Set Pos -> Pos -> S.Set Pos
exploreBlob arr loopPos blobPos p@(p1,p2) =
    if p `S.member` loopPos ||  p `S.member` blobPos then S.empty
    else aux S.empty p
    where
        ((minI, minJ), (maxI, maxJ)) = bounds arr
        aux visited p@(p1, p2) =
            if p `S.member` visited || p `S.member` loopPos then visited
            else foldl aux (S.insert p visited) neighbors
            where
                neighbors = [(p1+i, p2+j) | (i,j) <- [(-1,0),(1,0), (0,1), (0,-1)], p1+i >= 0, p2+j >= 0, p1+i <= maxI*2, p2+j <= maxJ*2]

-- part2PicksTheorem :: ((Int, Int), Array (Int, Int) Char) -> Int
part2PicksTheorem :: ((Int, Int), Array Pos Char) -> Int
part2PicksTheorem (s@(s1,s2), arr) = picksTheorem path
    where 
        path =  head . catMaybes . map (loop arr [s]) $ [(s1+1,s2), (s1, s2-1)]

trapezoidFormula :: [Pos] -> Int
trapezoidFormula ((x1,y1):(x2,y2):xs) = (y1+y2)*(x2-x1) + trapezoidFormula ((x2,y2):xs)
trapezoidFormula _ = 0

picksTheorem :: [Pos] -> Int
picksTheorem path =  area - b `div` 2 + 1
    where 
        area = trapezoidFormula path `div` 2 
        b = length path - 1

main :: IO ()
main = do
    input <- readFile "day10/input.txt"
    let parsed = parseInput input
    print "Day 10"
    -- print parsed
    timeIt $ print $ "Part 1: " ++ show (part1 parsed)
    timeIt $ print $ "Part 2: " ++ show (part2 parsed)
    timeIt $ print $ "Part 2 Pick's theorem: " ++ show (part2PicksTheorem parsed)

