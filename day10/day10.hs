module Main (main) where

import Data.List
import Data.Array (array, Array, (!), bounds, indices, listArray, assocs)
import qualified Data.Set as S
import Data.Maybe (catMaybes, fromJust, mapMaybe, isJust)
import System.TimeIt

type Pos = (Int, Int)
data Dir = U | D | L | R | Stop deriving (Show, Eq)

parseInput :: String -> (Pos, Array Pos Char)
parseInput input = (start, arr)
    where rows = lines input
          arr = listArray ((0,0), (length rows-1, length (head rows)-1)) $ concat rows
          start = fst . fromJust . find ((=='S').snd) $ assocs arr

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

loop :: Array Pos Char -> [Pos] -> Pos -> Maybe [Pos]
loop arr path@(prev:_) curr =
    if curr == last path then Just (curr:path)
    else do
        x <- move arr prev curr
        loop arr (curr: path) x

part1 :: (Pos, Array Pos Char) -> Int
part1 (s@(s1, s2), arr) =
    let (maxI, maxJ) = snd $ bounds arr
        starts = filter (inBounds arr) $ map (`moveInDir`s) [U,D,R,L]
    in (`div` 2) . length . head $ mapMaybe (loop arr [s]) starts


expandPath :: [Pos] -> [Pos]
expandPath ((x1,x2):(y1,y2):rest) = (x1*2,x2*2):(x1*2+y1-x1, x2*2+y2-x2):expandPath ((y1,y2):rest)
expandPath [(x1,x2)] = [(x1*2,x2*2)]

part2 :: ((Int, Int), Array (Int, Int) Char) -> Int
part2 (s@(s1, s2), arr) =
    S.size . S.difference allEvenPosSet . S.filter (\(i,j)-> even i && even j) $ S.union loopPos outBlobs
    where
        (maxI, maxJ) = snd $ bounds arr
        starts = filter (inBounds arr) $ map (`moveInDir`s) [U,D,R,L]
        allEvenPosSet = S.fromList . map (\(i,j)->(i*2,j*2)) $ indices arr
        loopPos = S.fromList . expandPath . head . mapMaybe (loop arr [s]) $ starts
        borderPos = [(i,j) | i <- [0..maxI*2], j<-[0,maxJ*2]] ++ [(i,j) | j <- [0..maxJ*2], i<-[0,maxI*2]] 
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
        path =  head . mapMaybe (loop arr [s]) $ [(s1+1,s2), (s1, s2-1)]

trapezoidFormula :: [Pos] -> Int
trapezoidFormula ((x1,y1):(x2,y2):xs) = (y1+y2)*(x2-x1) + trapezoidFormula ((x2,y2):xs)
trapezoidFormula _ = 0

picksTheorem :: [Pos] -> Int
picksTheorem path =
    let area = trapezoidFormula path `div` 2
        b = length path - 1
    in area - b `div` 2 + 1

main :: IO ()
main = do
    parsed <- parseInput <$> readFile "day10/input.txt"
    print "Day 10"
    timeIt $ print $ "Part 1: " ++ show (part1 parsed)
    timeIt $ print $ "Part 2: " ++ show (part2 parsed)
    timeIt $ print $ "Part 2 Pick's theorem: " ++ show (part2PicksTheorem parsed)