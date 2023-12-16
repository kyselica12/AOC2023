module Main where 

import Data.List.Split (splitOn)
import Data.List 
import qualified Data.Array as A
import qualified Data.Set as S
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

opositeDir :: Dir -> Dir
opositeDir D = U
opositeDir R = L
opositeDir U = D
opositeDir L = R

goBeam :: A.Array Pos Char -> S.Set (Pos, Dir) -> Pos -> Dir -> S.Set (Pos, Dir)
goBeam arr visited pos dir =
    if S.member (pos,dir) visited || not (inBounds pos arr) then visited 
    else case (arr A.! pos, dir) of
            ('.', _) -> goBeam arr (S.insert (pos,dir) visited) (move pos dir) dir
            ('/', R) -> goBeam arr (S.insert (pos,dir) visited) (move pos U) U 
            ('/', D) -> goBeam arr (S.insert (pos,dir) visited) (move pos L) L 
            ('/', L) -> goBeam arr (S.insert (pos,dir) visited) (move pos D) D 
            ('/', U) -> goBeam arr (S.insert (pos,dir) visited) (move pos R) R 
            ('\\', L) -> goBeam arr (S.insert (pos,dir) visited) (move pos U) U 
            ('\\', R) -> goBeam arr (S.insert (pos,dir) visited) (move pos D) D 
            ('\\', U) -> goBeam arr (S.insert (pos,dir) visited) (move pos L) L 
            ('\\', D) -> goBeam arr (S.insert (pos,dir) visited) (move pos R) R 
            ('-', R) -> goBeam arr (S.insert (pos,dir) visited) (move pos R) R 
            ('-', L) -> goBeam arr (S.insert (pos,dir) visited) (move pos L) L 
            ('-', _) -> let v' = (goBeam arr (S.insert (pos,dir) visited) (move pos L) L)
                        in (goBeam arr (S.insert (pos,dir) v') (move pos R) R)
            ('|', U) -> goBeam arr (S.insert (pos,dir) visited) (move pos U) U 
            ('|', D) -> goBeam arr (S.insert (pos,dir) visited) (move pos D) D 
            ('|', _) -> let v' = (goBeam arr (S.insert (pos,dir) visited) (move pos U) U)
                        in (goBeam arr (S.insert (pos,dir) v') (move pos D) D)

part1 arr = S.size $ S.map fst visited
    where
        visited = goBeam arr S.empty (0,0) R
        arr' = arr A.// [((i,j), '#') | (i,j) <- S.toList (S.map fst visited)]
        (mX,mY) = snd $ A.bounds arr'
        res = [[arr' A.! (i,j) | j <- [0..mY]] | i <- [0..mX]]
part2 arr =  maximum ( map (score R) [(i,0)| i<-[0..mX]] ++ map (score D) [(0,i)| i<-[0..mY]] ++ 
              map (score L) [(i,mY)| i<-[0..mX]] ++ map (score U) [(mX,i)| i<-[0..mY]])
    where
        score d s = trace (show s) S.size $ S.map fst (goBeam arr S.empty s d)
        (mX,mY) = snd $ A.bounds arr

showGrid :: [String] -> IO ()
showGrid grid = mapM_ putStrLn grid


main :: IO ()
main = do
    input <- readFile "day16/input.txt"
    let parsed = parseInput input
    print "Day 16"
    -- print parsed
    print $ "Part 1: " ++ (show $ part1 parsed)
    -- showGrid $ part1 parsed
    print $ "Part 2: " ++ (show $ part2 parsed)

