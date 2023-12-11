module Main (main) where

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
data Dir = U | D | L | R | Stop deriving (Show, Eq)

parseInput :: String -> (Pos, Array Pos Char)
parseInput input = (start, arr)
    where rows = lines input
          nr = length rows
          nc = length $ head rows
          arr = listArray ((0,0), (nr-1, nc-1)) $ concat rows
          start = fst . head . filter ((=='S').snd) $ assocs arr

getDir :: Pos -> Pos -> Dir
getDir s@(s1,s2) d@(d1,d2) = case (d1-s1, d2-s2) of 
    (1,0) -> D 
    (-1,0) -> U
    (0,1) -> R
    (0,-1) -> L
    _ -> Stop

moveInDir :: Dir -> Pos -> Pos
moveInDir U (x,y) = (x-1,y)
moveInDir D (x,y) = (x+1,y)
moveInDir L (x,y) = (x,y-1)
moveInDir R (x,y) = (x,y+1)

inBounds :: Array Pos Char -> Pos -> Bool
inBounds arr (x,y) = 
    let (maxI, maxJ) = snd (bounds arr)
    in x >= 0 && y >=0 && x <= maxI && y <= maxJ

move :: Array Pos Char -> Pos -> Pos -> Maybe Pos
move arr s@(s1, s2) d@(d1,d2) = 
    let outDir = pipeFlow (getDir s d)
        next = moveInDir outDir d
    in if outDir == Stop || not (inBounds arr next) then Nothing else Just next
    where 
        pipeFlow dir = case arr ! d of 
            '|' -> if dir == U || dir == D then dir else Stop
            '-' -> if dir == R || dir == L then dir else Stop
            'L' -> if dir == D then R else if dir == L then U else Stop
            'J' -> if dir == D then L else if dir == R then U else Stop
            '7' -> if dir == U then L else if dir == R then D else Stop
            'F' -> if dir == U then R else if dir == L then D else Stop

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
        -- trace (show prev ++ show curr ++ show (arr!curr) ++ "->" ++show x ++ show (move2 arr prev curr)) loop arr (curr: path) x
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

