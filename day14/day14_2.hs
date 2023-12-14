module Main where

import Data.List
import qualified Data.Map as M
import System.TimeIt (timeIt)

tiltRow :: String -> String
tiltRow xs = case span (/='#') xs of
    (a,[]) -> reverse $ sort a
    (a,b) -> reverse (sort a) ++ '#' : tiltRow (tail b)
    
tilt, tiltWest, tiltNorth, tiltEast, tiltSouth :: [String] -> [String]
tilt = map tiltRow
tiltWest = tilt
tiltNorth = transpose . tilt . transpose
tiltEast = map reverse . tilt . map reverse
tiltSouth = transpose . map reverse . tilt . map reverse . transpose

oneRound :: [String] -> [String]
oneRound = tiltEast . tiltSouth . tiltWest . tiltNorth

load :: [[Char]] -> Int
load input = sum [s-i | (i,r) <-zip [0..] input, x <- r, x == 'O', let s = length (head input)]

part1 :: [String] -> Int
part1 = load . tiltNorth

part2 :: [String] -> Int
part2 input = load $ aux M.empty 1000000000 input
    where
    aux _ 1 x = x
    aux m i x =
        let r = oneRound x
        in case M.lookup x m of
            Just v -> let d = v-i
                          n = i `div` d
                    in if n > 0 then aux m (i-n*d) r else aux m (i-1) r
            Nothing -> aux (M.insert x i m) (i-1) r

main :: IO ()
main = do
    input <- lines <$> readFile "day14/input.txt"
    timeIt $ print $ "Part 1: " ++ show (part1 input)
    timeIt $ print $ "Part 2: " ++ show (part2 input)