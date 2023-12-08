module Main where

import Data.List.Split (splitOn)
import Data.List
import qualified Data.Map as M

parseInput :: String -> (String, M.Map String (String, String))
parseInput content =
    let [moves, graph] = splitOn "\n\n" content
    in (moves,M.fromList $ map parseOne $ lines graph)
    where
        parseOne g =
            let [from, to] = splitOn " = " g
                [left, right] = splitOn ", " (tail $ init to)
            in (from, (left, right))

infiniteMoves :: String -> String
infiniteMoves moves = moves ++ infiniteMoves moves

move :: M.Map String (String, String) -> String -> String -> Int
move g (m:ms) node
    | last node == 'Z' = 0
    | m == 'L' = 1 + move g ms (fst $ g M.! node)
    | m == 'R' = 1 + move g ms (snd $ g M.! node)

countMoves :: String -> M.Map String (String, String) -> [String] -> [Int]
countMoves moves g = map (move g (infiniteMoves moves) )

part1 :: (String , M.Map String (String, String)) -> Int
part1 (moves,g) = move g (infiniteMoves moves) "AAA"

part2 :: (String , M.Map String (String, String)) -> Int
part2 (moves, g) = (foldl lcm 1 . countMoves moves g . filter (\x -> last x == 'Z' ))  (M.keys g)

main :: IO ()
main = do
    input <- readFile "day08/input.txt"
    let parsed = parseInput input
    print "Day 08"
    print $ "Part 1: " ++ show (part1 parsed)
    print $ "Part 2: " ++ show (part2 parsed)

