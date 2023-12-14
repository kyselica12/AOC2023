module Main where

import Data.List.Split (splitOn)
import Data.List
import qualified Data.Map as M
import Debug.Trace (trace)
import System.TimeIt (timeIt)

parseInput input = (size, map)
    where
        size = length $ lines input
        map = [ ((i,j), c) |
                (i, l) <- zip [0..] (lines input),
                (j, c) <- zip [0..] l,
                c /= '.']

rollNorth :: (Int, [((Int,Int),Char)]) -> (Int, [((Int,Int),Char)])
rollNorth (size,input) = (size,concatMap (aux size . col) [0..size-1])
    where
        aux :: Int -> [((Int,Int),Char)] -> [((Int,Int),Char)]
        aux v (((i,j),c):cs) = case c of
            'O' -> ((size-v,j),c) : aux (v-1) cs
            '#' -> ((i,j),c) : aux (size-i-1) cs
        aux v [] = []
        col i = sortOn (fst.fst) . filter ((==i).snd.fst) $ input

rollWest :: (Int, [((Int,Int),Char)]) -> (Int, [((Int,Int),Char)])
rollWest (size,input) =(size, concatMap (aux size . row) [0..size-1])
    where
        aux v (((i,j),c):cs) = case c of
            'O' -> ((i,size-v),c) : aux (v-1) cs
            '#' -> ((i,j),c) : aux (size-j-1) cs
        aux v [] = []
        row i = sortOn (snd.fst) . filter ((==i).fst.fst) $ input

rollEast :: (Int, [((Int,Int),Char)]) -> (Int, [((Int,Int),Char)])
rollEast (size,input) = (size, concatMap (aux size . row) [0..size-1])
    where
        aux v (((i,j),c):cs) = case c of
            'O' -> ((i,v-1),c) : aux (v-1) cs
            '#' -> ((i,j),c) : aux j cs
        aux v [] = []
        row i = sortOn ((*(-1)).snd.fst) . filter ((==i).fst.fst) $ input

rollSouth :: (Int, [((Int,Int),Char)]) -> (Int, [((Int,Int),Char)])
rollSouth (size,input) = (size ,concatMap (aux size . col) [0..size-1])
    where
        aux v (((i,j),c):cs) = case c of
            'O' -> ((v-1,j),c) : aux (v-1) cs
            '#' -> ((i,j),c) : aux i cs
        aux v [] = []
        col i = sortOn ((*(-1)).fst.fst) . filter ((==i).snd.fst) $ input

northLoad :: (Int, [((Int, b), Char)]) -> Int
northLoad (size, input) = sum $ map (\((i,j),_)-> size-i) $ filter ((=='O').snd) input

oneRound :: (Int, [((Int, Int), Char)]) -> (Int, [((Int, Int), Char)])
oneRound = rollEast . rollSouth . rollWest . rollNorth

part1 :: (Int, [((Int, Int), Char)]) -> Int
part1 = northLoad . rollNorth

part2 :: (Int, [((Int, Int), Char)]) -> Int
part2 input = northLoad $ aux M.empty 1000000000 input
    where
    aux _ 1 x = x
    aux m i x =
        let r = oneRound x
        in case M.lookup x m of
            Just v -> let d = v-i
                          n = i `div` d
                    in if n > 0 then 
                    trace ("Found cycle: "++show (1000000000-i)++" Len: "++show d) aux m (i-n*d) r 
                    else aux m (i-1) r
            Nothing -> aux (M.insert x i m) (i-1) r

main :: IO ()
main = do
    input <- parseInput <$> readFile "day14/input.txt"
    timeIt $ print $ "Part 1: " ++ show (part1 input)
    timeIt $ print $ "Part 2: " ++ show (part2 input)

