module Main where

import Data.List
import qualified Data.Map as M

tilt:: [String] -> [String]
tilt = transpose . map tiltRow . transpose
    where tiltRow xs = let (a,b) = span (/='#') xs in reverse (sort a) ++ if null b then [] else '#':tiltRow (tail b)

rotateRight :: [[a]] -> [[a]]
rotateRight = transpose . reverse

oneRound :: [String] -> [String]
oneRound input =  iterate (rotateRight.tilt) input !! 4

load :: [[Char]] -> Int
load input = sum [s-i | (i,r) <-zip [0..] input, x <- r, x == 'O', let s = length (head input)]

solve :: M.Map [String] Int -> Int -> [String] -> [String]
solve _ 1 x = x
solve m i x = let r = oneRound x
    in case (\v->v-i) <$> M.lookup x m of
        Just d -> if i >= d then solve m (i `mod` d) r else solve m (i-1) r
        Nothing -> solve (M.insert x i m) (i-1) r

main :: IO ()
main = do
    input <- lines <$> readFile "input.txt"
    print $ "Part 1: " ++ show ((load.tilt) input)
    print $ "Part 2: " ++ show ((load . solve M.empty 1_000_000_000) input)