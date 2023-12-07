 module Main where

import Data.List.Split (splitOn)
import Data.List
import qualified Data.Map as M
import Data.Maybe (isJust)
import Debug.Trace (trace)

data Card = Card {
    value :: Char
} deriving (Show)

data Hand = Hand {
    counter :: [(Card, Int)],
    cards :: [Card]
} deriving (Show)

-- -- implement Ord for Card
instance Eq Card where
    (Card v1) == (Card v2) = v1 == v2


instance Ord Card where
    (Card v1) `compare` (Card v2) = getVal v1 `compare` getVal v2
        where
            getVal c =
                case c of
                    'A' -> 14
                    'K' -> 13
                    'Q' -> 12
                    'T' -> 10
                    'J' -> 1
                    _ -> read [c] :: Int

instance Eq Hand where
    (Hand {counter=c1}) == (Hand {counter=c2}) = c1 == c2

showCards :: [Card] -> String
showCards = map value

instance Ord Hand where
    h1 `compare` h2 = 
        let pc = (priority h1) `compare` (priority h2) in
        if pc /= EQ then pc
        else compareCards (cards h1) (cards h2)
        where 
            priority c 
                | fiveOfAKind c = trace (show (showCards (cards c)) ++ " Five") 9
                | fourOfAKind c = trace (show (showCards (cards c)) ++ " Four") 8
                | fullHouse c = trace (show (showCards (cards c)) ++ " FullHouse")7
                | threeOfAKind c = trace (show (showCards (cards c)) ++ " Three")4
                | twoPairs c =trace (show (showCards (cards c)) ++ " TwoTwo") 3
                | pair c =trace (show (showCards (cards c)) ++ " Two") 2
                | highCard c =trace (show (showCards (cards c)) ++ " High") 1
                | otherwise = trace (show (showCards (cards c)) ++ " ----")0 
            compareCards (x:xs) (y:ys) = if x == y then compareCards xs ys else x `compare` y
            compareCards [] [] = EQ


fiveOfAKind :: Hand ->  Bool
fiveOfAKind h@(Hand co ca) = if snd (head co) == 5 then True else 
    let cJ = find (\(c, i) -> c == Card 'J') co
    in case cJ of
        (Just (_, 4)) -> True
        (Just (_, n)) -> let x = if n == 1 then [] else [(Card 'J', n-1)]
            in fourOfAKind (Hand ((filter ((/= Card 'J').fst) co)++x) ca)
        Nothing -> False

fourOfAKind :: Hand -> Bool
fourOfAKind h@(Hand co ca) = if snd (head co) == 4 then True else
    let cJ = find (\(c, i) -> c == Card 'J') co
    in case cJ of
        (Just (_, 3)) -> True
        (Just (_, n)) -> let x = if n == 1 then [] else [(Card 'J', n-1)]
            in threeOfAKind (Hand ((filter ((/= Card 'J').fst) co)++x) ca)
        Nothing -> False

threeOfAKind :: Hand -> Bool
threeOfAKind h@(Hand co ca) = if snd (head co) == 3 then True else
    let cJ = find (\(c, i) -> c == Card 'J') co
    in case cJ of
        (Just (_, 2)) -> True
        (Just (_, n)) -> let x = if n == 1 then [] else [(Card 'J', n-1)]
            in pair (Hand ((filter ((/= Card 'J').fst) co)++x) ca)
        Nothing -> False

twoPairs :: Hand -> Bool
twoPairs h@(Hand co ca) = if snd (head co) == 2 && pair (Hand (tail co) ca) then True else
    let cJ = find (\(c, i) -> c == Card 'J') co
    in case cJ of
        (Just (_, 2)) -> True
        (Just (_, 1)) -> snd (head co) == 2
        Nothing -> False

pair :: Hand -> Bool
pair h@(Hand co ca) = if snd (head co) == 2 then True else
    case find (\(c, i) -> c == Card 'J') co of
        (Just (_, 1)) -> True
        Nothing -> False
        _ -> error $ show co ++ "pair"

highCard :: Hand -> Bool
highCard h@(Hand co ca) = length co == 5


fullHouse :: Hand -> Bool
fullHouse h = let (x:y:xs) = counter h in if snd x == 3 && snd y == 2 then True else
    let rest = filter ((/= Card 'J').fst) (counter h)
    in 
    case find (\(c, i) -> c == Card 'J') (counter h) of
        (Just (_, 1)) -> twoPairs (Hand rest (cards h))
        (Just (_, 2)) -> pair (Hand rest (cards h))
        Nothing -> False
        x -> error $ show (counter h) ++ " " ++ show x ++ " fullHouse"


countOccurences :: [Char] -> Char -> Int
countOccurences cs c = length . filter (==c) $ cs

parseInput :: [Char] -> [(Hand, Int)]
parseInput content = 
    let (hands, values) = (\[x,y] -> (x,y))  $ transpose $ map words (lines content)
        hands' = nub $ map (\(xs, f) -> reverse $ sortOn snd $ map (\x-> (Card x, f x)) (nub xs)) $ map (\x -> (x, countOccurences x))  hands
        hands''= map (\(co, ca) -> Hand co (map Card ca)) $ zip hands' hands
    in zip hands'' (map read values :: [Int])



-- part1 :: [(Hand, Int)] -> [String]

part1 :: [(Hand, Int)] -> Int
part1 input = 
    let sortedV = map snd $ sortOn fst $ input
        sortedH = map fst $ sortOn fst $ input
        x = zipWith (*) sortedV [1..]

    -- in map (\x ->  (map (value)) $ cards x) sortedH 
    in sum  $ zipWith (*) sortedV [1..]

part2 :: a
part2 = undefined


main :: IO ()
main = do
    input <- readFile "day07/example.txt"
    let parsed = parseInput input
    print "Day 07"
    print $ part1 parsed
    -- print $ "Part 1: " ++ (show $ part1 parsed)
    print $ "Part 2: " ++ (show $ part1 parsed)
