module Main where
import Data.List.Split (splitOn)
import Data.List
import Data.Maybe (fromJust)
import Control.Applicative

-- count number of different elements in two lists 
difference :: ([String] , [String]) -> Int
difference (a,b) = (length .filter id . concat )(zipWith (zipWith (/=)) (reverse a) b)

-- solve for cols and rows
solveBlock :: (([String],[String]) -> Bool) -> [String] -> Int
solveBlock f block = fromJust $ 
    find (\i-> f (splitAt i (transpose block))) [1..length (head block)-1]
    <|> ((*100) <$> find (\i-> f (splitAt i block)) [1..length block-1])

main :: IO ()
main = do
    input <- map lines . splitOn "\n\n" <$> readFile "day13/input.txt"
    print $ "Part 1: " ++ show (sum $ map (solveBlock ((==0).difference)) input)
    print $ "Part 2: " ++ show (sum $ map (solveBlock ((==1).difference)) input)