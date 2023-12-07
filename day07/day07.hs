 module Main where

import Data.List.Split (splitOn)
import Data.List
import qualified Data.Map as M

data Card = Card {
    value :: Char
} deriving (Show)

data Hand = Hand {
    counter :: M.Map Card Int,
    cards :: [Card]
} deriving (Show)

-- implement Ord for Card
instance Eq Card where
    (Card v1) == (Card v2) = v1 == v2

allCards = [Card 'A', Card 'K', Card 'Q', Card 'J', Card 'T', Card '9', Card '8', Card '7', Card '6', Card '5', Card '4', Card '3', Card '2']
allCardsButJ = [Card 'A', Card 'K', Card 'Q', Card 'T', Card '9', Card '8', Card '7', Card '6', Card '5', Card '4', Card '3', Card '2']

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
    (Hand {counter=c1}) == (Hand {counter=c2}) = all (\x -> M.findWithDefault 0 x c1 == M.findWithDefault 0 x c2) allCards


instance Ord Hand where
    h1 `compare` h2 = 
        let pc = (priority h1) `compare` (priority h2) in
        if pc /= EQ then pc
        else compareCards (cards h1) (cards h2)
        where 
            priority c 
                | fiveOfAKind c = 9
                | fourOfAKind c = 8
                | fullHouse c = 7
                | threeOfAKind c = 4
                | twoPairs c = 3
                | pair c = 2
                | highCard c = 1
                | otherwise = 0 
            compareCards (x:xs) (y:ys) = if x == y then compareCards xs ys else x `compare` y
            compareCards [] [] = EQ


fiveOfAKind :: Hand ->  Bool
fiveOfAKind h@(Hand {counter=c}) = 
    let js = M.findWithDefault 0 (Card 'J') c
    in if any (\x -> M.findWithDefault 0 x c == 5) allCards then True 
    else case js of
        1 -> fourOfAKind (Hand {counter=M.insert (Card 'J') (js-1) c, cards=cards h})
        2 -> threeOfAKind (Hand {counter=M.insert (Card 'J') (js-2) c, cards=cards h})
        3 -> pair (Hand {counter=M.insert (Card 'J') (js-3) c, cards=cards h})
        4 -> True

fourOfAKind :: Hand -> Bool
fourOfAKind h@(Hand {counter=c}) = 
    let js = M.findWithDefault 0 (Card 'J') c
    in if any (\x -> M.findWithDefault 0 x c == 4) allCards then True 
    else case js of
        1 -> threeOfAKind (Hand {counter=M.insert (Card 'J') (js-1) c, cards=cards h})
        2 -> pair (Hand {counter=M.insert (Card 'J') (js-2) c, cards=cards h})
        3 -> True

threeOfAKind :: Hand -> Bool
threeOfAKind h@(Hand {counter=c}) = 
    let js = M.findWithDefault 0 (Card 'J') c
    in if any (\x -> M.findWithDefault 0 x c == 3) allCards then True
    else case js of
        1 -> pair (Hand {counter=M.insert (Card 'J') (js-1) c, cards=cards h})
        2 -> True

twoPairs :: Hand -> Bool
twoPairs h@(Hand {counter=c}) =
    let js = M.findWithDefault 0 (Card 'J') c
    in length (filter (\x -> M.findWithDefault 0 x c == 2) allCards) == 2 then True
    else case js of
        1 -> pair (Hand {counter})
        2 -> True

pair :: Hand -> Bool
pair (Hand {counter=c}) = 
    let js = M.findWithDefault 0 (Card 'J') c
    in if any (\x -> M.findWithDefault 0 x c == 2) allCards then True
    else case js of
        1 -> True

fullHouse :: Hand -> Bool
fullHouse h = threeOfAKind h && pair h

highCard :: Hand -> Bool
highCard h = all (\x -> M.findWithDefault 1 x (counter h) == 1) allCards


-- parseInput :: [Char] -> [[]]
parseInput content =
    let (hands, values) = (\[x,y] -> (x,y))  $ transpose $ map words (lines content)
        hands' = map  (map (\x -> Card {value=x})) hands
    in (map (parseHand M.empty) hands', map read values :: [Int])
    where 
        -- parseHand :: M.Map Card Int -> [String] -> Hand
        parseHand m xs = 
            let x = foldl (\m' x -> M.insert x ((M.findWithDefault 0 x m')+1) m' ) m xs
            in Hand {counter=x, cards=xs}

part1 :: ([Hand], [Int]) -> Int
part1 (hands, values) = 
    let sortedV = map snd $ sortOn fst $ zip hands values
    in sum $ zipWith (*) sortedV [1..]

part2 = undefined


main :: IO ()
main = do
    input <- readFile "day07/input.txt"
    let parsed = parseInput input
    print "Day 07"
    print $ "Part 1: " ++ (show $ part1 parsed)
    -- print $ "Part 2: " ++ (show $ part2 parsed)

