module Main where

next :: [Int] -> Int
next s@(x:xs) = if any (/=0) s then x + next (zipWith (-) s (tail s)) else x

main :: IO ()
main = do
    parsed <- map (map read . words) . lines <$> readFile "day09/input.txt"
    print $ "Part 1: " ++ show ((sum . map next . reverse) parsed)
    print $ "Part 2: " ++ show ((sum . map next) parsed)