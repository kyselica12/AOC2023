module Main where
import Data.List ( sortOn )

type Pos = (Int, Int)

parseInput :: String -> [(Int, Int)]
parseInput content = [(i,j) | (i,xs) <- zip [0..] (lines content), (j,x) <- zip [0..] xs, x == '#' ]

updateStar :: Bool -> Int -> Pos -> Pos
updateStar updateRow d (x,y)
    | updateRow  = (x+d,y)
    | otherwise  = (x,y+d)

expandSpace :: Int -> [Pos] -> [Pos]
expandSpace scale = aux snd (updateStar False) 0 . sortOn snd . aux fst (updateStar True) 0 . sortOn fst
    where
        aux f uf e (x:y:xs)
            |  f y - f x <= 1 = uf e x:aux f uf e (y:xs)
            | otherwise =  uf e x:aux f uf (e+(f y-f x-1)*(scale -1)) (y:xs)
        aux f uf e [x] = [uf e x]

dist :: [Pos] -> Int
dist ((x,y):xs) = (sum .map (\(x',y') -> abs (x-x') + abs (y-y'))) xs + dist xs
dist [] = 0

main :: IO ()
main = do
    parsed <- parseInput <$> readFile "day11/input.txt"
    print $ "Part 1: " ++ show ((dist.expandSpace 2) parsed)
    print $ "Part 2: " ++ show ((dist.expandSpace 1000000) parsed)