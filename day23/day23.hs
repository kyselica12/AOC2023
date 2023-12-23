module Main where

import Data.List.Split (splitOn)
import Data.List
import qualified Data.Array as A
import qualified Data.Set as S
import qualified Data.Map as M
import Debug.Trace (trace)
import System.TimeIt (timeIt)
import Data.Maybe (fromJust)
import qualified Data.PQueue.Prio.Min as PQ

data Dir = D | U | L | R deriving (Show, Eq, Ord)

parseInput :: [Char] -> A.Array (Int, Int) Char
parseInput input = arr
    where
        rows = lines input
        nr = length rows
        nc = length $ head rows
        arr = A.array ((0,0),(nr-1,nc-1)) [((r,c), x) | (r, row) <- zip [0..] rows, (c, x) <- zip [0..] row]

getNeighbors' :: A.Array (Int, Int) Char -> (Int,Int) -> [(Int,Int)]
getNeighbors' arr (r,c) = filter ((/='#').(arr A.!)) $ case arr A.! (r,c) of
    '>' -> [(r,c+1)]
    '<' -> [(r,c-1)]
    '^' -> [(r-1,c)]
    'v' -> [(r+1,c)]
    _ -> [(r,c+1), (r,c-1), (r-1,c), (r+1,c)]

getNeighbors :: A.Array (Int, Int) Char -> (Int,Int) -> [(Int,Int)]
getNeighbors arr (r,c) = filter (\p-> inBounds p && arr A.! p /= '#') [(r,c+1), (r,c-1), (r-1,c), (r+1,c)]
    where inBounds (r,c) = r >= 0 && c >= 0 && r <= maxR && c <= maxC
          (maxR, maxC) = snd $ A.bounds arr

findCrossRoads :: A.Array (Int, Int) Char -> [(Int,Int)]
findCrossRoads arr = filter (\(r,c) -> arr A.! (r,c) /= '#' && length (getNeighbors arr (r,c)) > 2) [(r,c) | r <- [1..maxR-1], c <- [1..maxC-1]]
    where (maxR, maxC) = snd $ A.bounds arr

createGraph :: A.Array (Int, Int) Char -> [(Int,Int)] -> [[(Int,Int)]] -> S.Set (Int,Int) -> [((Int,Int),(Int,Int),Int)]
createGraph arr crs queue visited
    | null queue = []
    | (r,c) `elem` crs && not (null xs) = (last path, (r,c), l-1) : createGraph arr crs (tail queue) visited'
    | S.member (r,c) visited = createGraph arr crs queue' visited
    | otherwise = createGraph arr crs queue' visited
    where
        (maxR, maxC) = snd $ A.bounds arr
        path@((r,c):xs) = head queue
        neighbours = filter (\p -> S.notMember p visited && p`notElem` path) $  getNeighbors arr (r,c)
        paths = map (:path) neighbours
        l = length path
        queue' = paths++tail queue
        visited' = S.insert (head xs) visited

solve :: A.Array (Int, Int) Char -> Int -> [[(Int,Int)]] -> Int
solve arr  best queue
    | null queue = best
    | (maxR,maxC-1) `elem` neighbours = solve arr (max best l) (tail queue)
    | otherwise = solve arr best (tail queue++paths)
    where
        (maxR, maxC) = snd $ A.bounds arr
        path@((r,c):xs) = head queue
        neighbours = filter (`notElem` path) $  getNeighbors' arr (r,c)
        paths = map (:path) neighbours
        l = length path
        blockEnd =  all (\i -> any ((==i).fst) path) [1..maxR-1]

solveGraph :: (Int, Int) -> M.Map (Int, Int) [((Int, Int), Int)] -> Int -> PQ.MinPQueue Int [(Int, Int)] -> Int
solveGraph end mem best queue
    | null queue = best
    | end `elem` neighborPositions =
        let (p',v') = fromJust $ find (\(p,_) -> p == end) neighbours
            l' = (v' + l)
        in solveGraph end mem (max best l') (PQ.deleteMin queue)
    | otherwise = solveGraph end mem best queue'
    where
        (l, path@((r,c):xs)) = fromJust $ PQ.getMin queue
        neighbours = filter (\(p,v)-> p `notElem` path) $ mem M.! (r,c)
        queue' = foldl (\q (p,v) -> PQ.insert (v+l) (p:path) q) (PQ.deleteMin queue) neighbours
        neighborPositions = map fst neighbours

part1 :: A.Array (Int, Int) Char -> Int
part1 arr = solve arr' 0 [[(1,1),(0,1)]]
    where
        (maxR, maxC) = snd $ A.bounds arr
        end = (maxR, maxC-1)
        arr' = arr A.// [((0,1),'v')]

part2 :: A.Array (Int, Int) Char -> Int
part2 arr = solveGraph (maxR,maxC-1) mem 0 (PQ.singleton 0 [(0,1)])
    where
        dists = createGraph arr crs' queue S.empty
        (maxR, maxC) = snd $ A.bounds arr
        crs = findCrossRoads arr
        crs' = (0,1):(maxR,maxC-1):crs
        queue = concatMap (\p -> map (\n-> [n,p]) (getNeighbors arr p) ) crs
        mem = M.fromList $ map createMapEntry $ groupBy (\(a,_,_) (a',_,_) -> a==a') $ sort $ map getMax $
               groupBy (\(a,b,_) (a',b',_) -> a==a' && b==b') $ sort $ dists ++ map (\(a,b,c)->(b,a,c)) dists
        getMax g@((a,b,c):_) = (a,b,maximum $ map (\(_,_,c)->c) g)
        createMapEntry g@((a,b,c):_) = (a,map (\(_,b',c') -> (b',c')) g )

main :: IO ()
main = do
    input <- readFile "day23/input.txt"
    let parsed = parseInput input
    print "Day 23"
    timeIt $ print $ "Part 1: " ++ show (part1 parsed)
    timeIt $ print $ "Part 2: " ++ show (part2 parsed)