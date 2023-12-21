module Main where

import Data.List.Split (splitOn)
import Data.List
import qualified Data.Array as A
import qualified Data.Set as S
import Debug.Trace (trace)

type Pos = (Int, Int)
data Dir = N | E | S | W deriving (Show, Eq, Ord)
parseInput input = (arr, start)
    where
        rows = lines input
        nr = length rows
        nc = length $ head rows
        arr = A.array ((0,0), (nr-1,nc-1)) [((r,c),x) | (r,row) <- zip [0..] rows, (c,x) <- zip [0..] row]
        start = head [(r,c) | r <- [0..nr-1], c <- [0..nc-1], arr A.! (r,c) == 'S']


move :: Dir -> Pos -> Pos
move dir (r,c) = case dir of
    N -> (r-1,c)
    E -> (r,c+1)
    S -> (r+1,c)
    W -> (r,c-1)

inBounds :: A.Array Pos Char -> Pos -> Bool
inBounds arr (r,c) = r >= 0 && r <= nr && c >= 0 && c <= nc
    where (nr,nc) = snd $ A.bounds arr

oneStep :: A.Array Pos Char -> [Pos] -> [Pos]
oneStep arr queue = next
    where
        next = nub $ filter (\p -> (inBounds arr p) && arr A.! p /= '#') $ concatMap (\p -> map (`move` p) [N,E,S,W]) queue

part1 :: (A.Array Pos Char, Pos) -> Int
part1 (arr, start) =length $ map (arr A.!) $ iterate (oneStep arr) [start] !! 64

moveInf :: A.Array Pos Char -> Pos -> [Pos]
moveInf arr (r,c) =
    let res = filter (\p -> arr A.! modPos p /= '#') $ map (`move` (r,c)) [N,E,S,W]
    -- in trace (show ((r,c), modPos (r,c),res, map modPos res)) res
    in res
    where modPos (r,c) = (r `mod` (nr+1), c `mod` (nc+1))
          (nr,nc) = snd $ A.bounds arr

modPos :: A.Array Pos Char -> Pos -> Pos
modPos arr (r,c) = (r `mod` (nr+1), c `mod` (nc+1))
    where (nr,nc) = snd $ A.bounds arr

oneStepInf arr visited pos = xxxx
    where res = S.fromList $  concatMap (moveInf arr) pos
          xxxx = S.difference (S.fromList (concatMap (moveInf arr) pos)) visited

part2 (arr, start) = result
    where
        size = 131
        halfSize = size `div` 2
        steps = 26501365
        fill step flag visited pos score acc =
            let next = oneStepInf arr visited pos
                count = length next
                score' = if flag then score + count else score
                acc' = count : acc
            in if step == 0 then (score, reverse acc, pos, visited, flag)
            else  fill (step-1) (not flag) pos next score' acc'

        (s, _,p,v,f) = fill halfSize True S.empty (S.fromList [start]) 0 []
        (s',a',p',v',f') = fill size  f v p s []
        (s'',a'',p'',v'',f'') = fill size  f' v' p' s' []
        delta = zipWith (-) a'' a'
        (_, result) = aux (steps - halfSize - (2*size)) delta a'' f'' s''

        interpolateCycle delta prev flag score =
            let next = zipWith (+) delta prev
                (f,score') = foldl' (\(f,s) x -> if f then (not f, s+x) else (not f, s)) (flag, score) next
            in (next, score',f)

        aux step delta prev flag score =
            let (next, score',f) = interpolateCycle delta prev flag $! score
            in if step < size then (step, score)
            else aux (step-size) delta next f score'



main :: IO ()
main = do
    input <- readFile "day21/input.txt"
    let parsed = parseInput input
    print "Day 21"
    print $ "Part 1: " ++ show (part1 parsed)
    print $ "Part 2: " ++ show (part2 parsed)
