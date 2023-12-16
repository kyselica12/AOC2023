module Main where

import Data.List.Split (splitOn)
import Data.List
import qualified Data.Array as A
import qualified Data.Set as S
import System.TimeIt (timeIt)
import Debug.Trace (trace)

type Pos = (Int, Int)
data Dir = D | R | U | L deriving (Show, Eq, Ord)

-- implement Ord for Pos
parseInput input = arr
    where
        n = length $ lines input
        arr = A.array ((0,0), (n-1, n-1)) [((i,j),c) |
                                            (i,l) <- zip [0..] (lines input),
                                            (j,c) <- zip [0..] l]

move :: Pos -> Dir -> Pos
move (x,y) D = (x+1, y)
move (x,y) R = (x, y+1)
move (x,y) U = (x-1, y)
move (x,y) L = (x, y-1)

inBounds :: Pos -> A.Array Pos Char -> Bool
inBounds (x,y) arr = x >= 0 && y >= 0 && x <= mX && y <= mY
    where (mX,mY) = snd $ A.bounds arr

goBeam :: A.Array Pos Char -> Pos -> Dir -> S.Set (Pos, Dir) -> S.Set (Pos, Dir)
goBeam arr pos dir visited =
    if S.member (pos,dir) visited || not (inBounds pos arr) then visited
    else case (arr A.! pos, dir) of
            ('.', _) -> goBeam arr (move pos dir) dir (S.insert (pos,dir) visited)
            ('/', R) -> goBeam arr (move pos U) U (S.insert (pos,dir) visited)
            ('/', D) -> goBeam arr (move pos L) L (S.insert (pos,dir) visited)
            ('/', L) -> goBeam arr (move pos D) D (S.insert (pos,dir) visited)
            ('/', U) -> goBeam arr (move pos R) R (S.insert (pos,dir) visited)
            ('\\', L) -> goBeam arr (move pos U) U (S.insert (pos,dir) visited)
            ('\\', R) -> goBeam arr (move pos D) D (S.insert (pos,dir) visited)
            ('\\', U) -> goBeam arr (move pos L) L (S.insert (pos,dir) visited)
            ('\\', D) -> goBeam arr (move pos R) R (S.insert (pos,dir) visited)
            ('-', R) -> goBeam arr  (move pos R) R (S.insert (pos,dir) visited)
            ('-', L) -> goBeam arr  (move pos L) L (S.insert (pos,dir) visited)
            ('-', _) -> goBeam arr (move pos L) L (
                         goBeam arr  (move pos R) R (S.insert (pos,dir) visited))
            ('|', U) -> goBeam arr  (move pos U) U (S.insert (pos,dir) visited)
            ('|', D) -> goBeam arr  (move pos D) D (S.insert (pos,dir) visited)
            ('|', _) -> goBeam arr  (move pos U) U (goBeam arr  (move pos D) D (S.insert (pos,dir) visited))

part1,part2 :: A.Array Pos Char -> Int
part1 arr = S.size $ S.map fst $ goBeam arr (0,0) R S.empty
part2 arr =  maximum ( [score R (i, 0) | i <- [0 .. mX]] ++ [score D (0, i) | i <- [0 .. mY]] ++
              [score L (i, mY) | i <- [0 .. mX]] ++ [score U (mX, i) | i <- [0 .. mY]])
    where
        score d s = S.size $ S.map fst (goBeam arr s d S.empty)
        (mX,mY) = snd $ A.bounds arr

main :: IO ()
main = do
    input <- readFile "day16/input.txt"
    let parsed = parseInput input
    print "Day 16"
    timeIt $ print $ "Part 1: " ++ (show $ part1 parsed)
    timeIt $ print $ "Part 2: " ++ (show $ part2 parsed)

