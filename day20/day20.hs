module Main where

import Data.List.Split (splitOn)
import Data.List
import Text.Parsec hiding (State)
import Debug.Trace (trace)
import Language.Haskell.TH (isInstance)

data State = B | F {on::Bool} | C {mem::[Bool]} deriving (Show, Eq)

data Module = M {name::String, to::[String], s::State} deriving (Show, Eq)

-- inputP :: Parsec String () [String]
inputP = do
    lines <- sepEndBy moduleP (char '\n')
    eof
    return lines

moduleP = do
    t <- lookAhead (anyToken)
    trace (show t) $ return ()
    name <- many1 (noneOf " ")
    string " -> "
    x <- many1 (noneOf "\n")
    return $ construcsModule name t x
    where
        getName t name= if t == '%' || t == '&' then tail name else name
        construcsModule name t x =
            let to = splitOn ", " x
            in case t of
                '%' -> M (getName t name) to (F False)
                '&' -> M (getName t name) to (C [])
                _ -> M (getName t name) to B



parseInput input =
    let parsed = either (error.show) id (parse inputP "ERROR" input)
        cons = map name $ filter (\(M _ _ s) -> s /= B && s /= F False) parsed
        other = filter ((`notElem` cons).name) parsed
    in trace (show other) parsed


part1 = undefined

part2 = undefined

main :: IO ()
main = do
    -- input <- readFile "day20/input.txt"
    input <- readFile "day20/example.txt"
    let parsed = parseInput input
    print "Day 20"
    print parsed
    -- print $ "Part 1: " ++ (show $ part1 parsed)
    -- print $ "Part 2: " ++ (show $ part2 parsed)

