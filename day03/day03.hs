module Main where

import Data.List
import Data.List.Split
import Data.Char

import Data.Set as Set hiding (null, filter, foldl, map)
import Debug.Trace (trace)
import qualified Data.Map as Map
import GHC.Plugins (notNull)
import Data.Maybe


type Pos = (Int, Int)


stripNonDigits :: [(Char, Int)] -> [(Char, Int)]
stripNonDigits = dropWhile (not.isDigit.fst)

firsts :: [(a, b)] -> [a]
firsts = map fst

parseNumbers :: [String] -> [(Int, Pos)]
parseNumbers = parseInput isDigit read

parseCharacters :: [String] -> [(Char, Pos)]
parseCharacters = parseInput (\x -> x /= '.' && not (isDigit x)) (\x -> head x)

parseInput :: (Char -> Bool) -> ([Char] -> a) -> [[Char]] -> [(a, Pos)]
parseInput f g lines =
    let r' = aux 1 $ zip (head lines) [0..]
        x = zip [0..] lines
    in concatMap (\(i, l) -> aux i (zip l [0..])) (zip [0..] lines)
    where
        nums = span (f.fst)
        aux j l = case span (f.fst) l of
            ([], r) ->  if null r then [] else aux j (strip r)
            (z, []) ->  [(g (firsts z), (j, snd $ last z))]
            (z, r) ->  (g (firsts z), (j, snd $ last z)):aux j (strip r)
        strip = dropWhile (not.f.fst)

getNeighbors :: (Int, (Int, Int)) -> [(Int, Int)]
getNeighbors (n, (i, j)) =  allNeighbors i j n
    where
        neighbors x y =  [(x-1,y), (x+1, y), (x, y+1), (x,y-1), (x+1,y+1), (x-1,y-1), (x+1, y-1), (x-1, y+1)]
        aux i j' r
            | r `div` 10 > 0 || r > 0 = neighbors i j' ++ aux i (j'-1) (r `div` 10)
            | otherwise = []
        allNeighbors i j n = nub (aux i j n)

part1 lines =
    let nums  = parseNumbers lines
        setChars = Set.fromList $ map snd $ parseCharacters lines
        ok = filter (any (`Set.member` setChars) . getNeighbors) nums
    in (sum . firsts) ok

part2 lines =
    let nums = parseNumbers lines
        gears = parseInput (=='*') id lines
        gearMap = Map.fromList $ zip (map snd gears) (repeat [])
        foundGears = map (\(n, p) -> (find (`Map.member` gearMap) (getNeighbors (n, p)), n)) nums
    in (Map.foldl (+) 0 . 
        Map.map product . 
        Map.filter ((==2).length) . 
        foldl (\m (p, v) -> Map.adjust (v:) (fromJust p) m)  gearMap  .
        filter (isJust.fst)) foundGears




main :: IO ()
main = do
    lines <- lines <$> readFile "input.txt"
    print $ "Part1: " ++ show (part1 lines)
    print $ "Part2: " ++ show (part2 lines)