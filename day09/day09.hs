module Main where

next :: [Int] -> Int
next s@(x:xs) = if any (/=0) s then x + next (zipWith (-) s (tail s)) else x

main :: IO ()
main = do
    parsed <- map (map read . words) . lines <$> readFile "day09/input.txt"
    print $ "Part 1: " ++ show (foldl (\a x -> a + next (reverse x)) 1 parsed)
    print $ "Part 2: " ++ show (foldl (\a x -> a + next x) 1 parsed)