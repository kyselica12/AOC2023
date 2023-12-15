module Main where 

import Data.List.Split (splitOn)
import Data.List 
import Data.Char (ord)
import Data.Text (strip)
import Debug.Trace (trace)

parseInput = splitOn "," . head . lines

computeHash :: String -> Int
computeHash = foldl (\a c -> (a + ord c)*17 `mod` 256) 0
computeHash' = foldl (\a c -> (a + ord c)*17 `mod` 256) 0 . takeWhile (\c -> c/='-' && c /='=')
part1 = sum . map computeHash

-- part2 :: [[Char]] -> Int
part2 input = sum $ map (score.(aux [])) boxes
    where 
        boxes = groupBy (\x y -> snd x == snd y) $ sortOn (snd) $ map (\x-> (x, computeHash' x)) input
        aux acc ((b,_):bs) = 
            let a = dropWhile (\x-> x /= '-' && x /= '=') b
                n = takeWhile (\x-> x /= '-' && x /= '=') b
                v = read (tail a) :: Int
            in case findIndex ((==n).fst) acc of 
                Just i -> if head a == '-' then aux (take i acc ++ drop (i+1) acc) bs
                          else aux (take i acc ++ [(n,v)] ++ drop (i+1) acc) bs
                Nothing -> if head a == '-' then aux acc bs 
                           else aux (acc ++ [(n,v)]) bs
        aux acc [] = acc
        score box = foldl (\a ((x,(n,v))) -> let h = computeHash' n +1 in (a+h*x*v)) 0 $ zip [1..] box



main :: IO ()
main = do
    input <- readFile "day15/input.txt"
    let parsed = parseInput input
    print "Day 15"
    print parsed
    -- print $ "Part 1: " ++ (show $ part1 parsed)
    print $ "Part 2: " ++ (show $ part2 parsed)

