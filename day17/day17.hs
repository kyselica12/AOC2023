module Main where

import Data.List.Split (splitOn)
import Data.List
import qualified Data.Array as A
import qualified Data.Set as S
import System.TimeIt (timeIt)
import Debug.Trace (trace)
import qualified Data.PQueue.Prio.Min as PQ
import Data.Maybe (fromJust)

data Dir = U | D | L | R deriving (Show, Eq, Ord)
type Pos = (Int, Int)
type State = (Int, Pos, Dir)
type PQueue = [State]

enqueue :: PQueue -> [State] -> PQueue
enqueue q add = sortOn (\(c,(x,y),_) -> c-x-y) $ q ++ add


parseInput :: [Char] -> A.Array (Int, Int) Int
parseInput input = A.array ((0,0),(length ls -1, length (head ls) -1))
                   [((x,y), read [c] :: Int) | (x, l) <- zip [0..] ls, (y,c) <- zip [0..] l]
    where
        ls = lines input

inBounds :: A.Array (Int, Int) Int -> Pos -> Bool
inBounds arr (x,y) = x >= 0 && y >= 0 && x <=x' && y <= y'
    where (x',y') = snd $ A.bounds arr

getNeighbors :: Int -> Int -> A.Array (Int, Int) Int -> State -> [State]
getNeighbors min max arr (c, (x,y), dir) = case dir of
    U -> left ++ right
    D -> left ++ right
    L -> up ++ down
    R -> up ++ down
    where
        down = drop (min-1) $ collect D c [(x+i,y) | i<-[1..max], inBounds arr (x+i,y)]
        up = drop (min-1) $ collect U c [(x-i,y) | i<-[1..max], inBounds arr (x-i,y)]
        left = drop (min-1) $ collect L c [(x,y-i) | i<-[1..max], inBounds arr (x,y-i)]
        right = drop (min-1) $ collect R c [(x,y+i) | i<-[1..max], inBounds arr (x,y+i)]
        collect dir curr (p:ps)= (curr+arr A.! p, p, dir) : collect dir (curr+arr A.! p) ps
        collect _ _ [] = []


solve :: A.Array (Int, Int) Int -> PQ.MinPQueue Int (Pos,Dir) -> S.Set (Pos, Dir) -> Int
solve arr queue visited =
    let (c, ((x,y), dir)) = fromJust $ PQ.getMin queue
        neighbours = filter (\(_,p,d') -> S.notMember (p,d') visited) $ getNeighbors 4 10 arr (c,(x,y),dir)
        queue' = foldl (\q (c',p,d') -> PQ.insert c' (p,d') q) (PQ.deleteMin queue) neighbours
    in if x == maxX && y == maxY then c
    else  if S.member ((x,y),dir) visited then solve arr (PQ.deleteMin queue) visited
    else solve arr queue' (S.insert ((x,y),dir) visited) 
    where (maxX, maxY) = snd $ A.bounds arr

part1 arr = solve arr queue S.empty
    where 
        (maxX, maxY) = snd $ A.bounds arr
        queue = PQ.insert 0 ((0,0),L) $ PQ.insert 0 ((0,0),D) PQ.empty

part2 = undefined


main :: IO ()
main = do
    input <- readFile "day17/input.txt"
    let parsed = parseInput input
    print "Day 17"
    timeIt $ print $ "Part 1: " ++ (show $ part1 parsed)
    -- print $ "Part 2: " ++ (show $ part2 parsed)

