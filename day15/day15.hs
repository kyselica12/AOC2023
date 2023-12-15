module Main where

import Data.List.Split (splitOn)
import Data.List
import Data.Char (ord, isSpace)

computeHash, computeHash2 :: String -> Int
computeHash = foldl (\a c -> (a + ord c)*17 `mod` 256) 0
computeHash2 = computeHash . takeWhile (\c -> c /= '-' && c /= '=')

part2 :: [String] -> Int
part2 = sum . map (score.aux []) . createBoxes
    where
        createBoxes = groupBy (\x y -> snd x == snd y) . sortOn snd . map (\x-> (x, computeHash2 x))
        aux acc ((b,h):bs) =
            let (n,a) = span (\c -> c /= '-' && c /= '=') b
            in case findIndex ((==n).fst) acc of
                Just i -> if head a == '-' then aux (take i acc ++ drop (i+1) acc) bs
                          else aux (take i acc ++ [(n,(h+1)*read (tail a))] ++ drop (i+1) acc) bs
                Nothing -> if head a == '-' then aux acc bs
                           else aux (acc ++ [(n,(h+1) *read (tail a))]) bs
        aux acc [] = acc
        score box = foldl (\a (x,(n,v)) -> a+x*v) 0 $ zip [1..] box

main :: IO ()
main = do
    input <- splitOn "," <$> readFile "day15/input.txt"
    print $ "Part 1: " ++ show (sum $ map computeHash input)
    print $ "Part 2: " ++ show (part2 input)