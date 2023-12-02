module Main where 

import Data.List
import Debug.Trace
import Data.Char
import Data.List.Split


parseInput :: String -> [[(Int, String)]]
parseInput line = tuples [] words'  
    where 
        words' = tail $ tail $ words line
        tuples acc (x:y:xs) = let acc' = (read x, y):acc in if (last y) == ';' then  acc':tuples [] xs else tuples acc' xs
        tuples acc [] = [acc]
        groups = splitOn ";" line

count :: Int -> Int -> Int -> [(Int, String)] -> Bool
count r g b ((n,y):xs)  
    | r < 0 || g < 0 || b < 0 = False
    | "red" `isInfixOf` y = count (r-n) g b xs 
    | "blue" `isInfixOf` y = count r g (b-n) xs 
    | "green" `isInfixOf` y = count r (g-n) b xs 
    | otherwise = trace ("OTHERWISE") False
count r g b [] = r >=0 && g >= 0 && b >= 0

count2 ::  [[(Int, String)]] -> Int
count2 parsed = let (r, g, b) = max' in  r*g*b 
    where aux r g b ((n,y):xs) 
            | "red" `isInfixOf` y = aux (max r n) g b xs 
            | "blue" `isInfixOf` y = aux r g (max n b) xs 
            | "green" `isInfixOf` y = aux r (max n g) b xs 
            | otherwise = trace ("OTHERWISE") (-1,-1,-1)
          aux r g b [] = (r,g,b)
          max' = foldl (\(r,g,b) (r',g',b') -> (max r r', max g g', max b b')) (0,0,0) $ map (aux 0 0 0) parsed


main :: IO ()
main = do 
    lines <- lines <$> readFile "input.txt"
    let parsed = map parseInput lines 
    let counted = map ( map (count 12 13 14)) parsed 
    let part1 = foldl (\acc (i, vs) -> if all (==True) vs then trace (show (i, vs)) acc+i else acc) 0 (zip [1..] counted)
    let part2 = sum $ map count2 parsed
    -- print $ last parsed
    -- print $ last counted
    print $ "Part1: " ++ show part1
    print $ "Part2: " ++ show part2