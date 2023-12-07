module Main where

import Data.List
import Debug.Trace (trace)

value :: Char -> Int
value c = case c of
    'A' -> 14
    'K' -> 13
    'Q' -> 12
    'J' -> 11
    'T' -> 10
    _ -> read [c] :: Int

value' :: Char -> Int
value' c = case c of
    'J' -> 0
    n -> if n == 'J' then error "JJJJ" else value n

createHistogram :: [Char] -> [(Char, Int)]
createHistogram = sortOn snd . map (\x-> (head x, length x)) . group . sort

handValue :: (String, Int) -> Int
handValue input = case map snd . createHistogram.fst $ input of
    [5] -> 9
    [1,4] -> 8
    [2,3] -> 7
    [1,1,3] -> 4
    [1,2,2] -> 3
    [1,1,1,2] -> 2
    [1,1,1,1,1] -> 1

handValue' :: (String, Int) -> Int
handValue' (hand, bid) =
    let hist = createHistogram hand
        j = find ((== 'J').fst) hist
    in case j of
        Nothing -> handValue (hand, bid)
        Just (_, n) ->
            let h' = filter (/='J') hand
                hist' = createHistogram h'
                app = replicate n (fst $ last hist')
            in case hist' of
                [] -> handValue (hand, bid)
                _ -> handValue (h'++app, bid)

compareHands :: (String, Int) -> (String, Int) -> Ordering
compareHands (h1,_) (h2,_) =
    let v1 = handValue (h1,0)
        v2 = handValue (h2,0)
    in if v1 == v2 then map value h1 `compare` map value h2 else v1 `compare` v2

compareHands' :: (String, Int) -> (String, Int) -> Ordering
compareHands' (h1,_) (h2,_) =
    let v1 = handValue' (h1,0)
        v2 = handValue' (h2,0)
    in if v1 == v2 then map value' h1 `compare` map value' h2 else v1 `compare` v2

part1 :: [(String, Int)] -> Int
part1 input = sum $ zipWith (*) [1..] (map snd $ sortBy compareHands input)

part2 :: [(String, Int)] -> Int
part2 input = sum $ zipWith (*) [1..] (map snd $ sortBy compareHands' input)

parseInput :: String -> [(String, Int)]
parseInput = map ((\[x,y]->(x, read y)).words) . lines
main :: IO ()
main = do
    content <- readFile "day07/input.txt"
    let parsed = parseInput content
    print $ part1  parsed
    print $ part2  parsed