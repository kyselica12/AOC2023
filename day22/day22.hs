module Main where

import Data.List.Split (splitOn)
import Data.List
import Debug.Trace (trace)
import qualified Data.Map as M

data Block = Block { x :: Range, y:: Range, z :: Range} deriving (Show, Eq, Ord)
data Range = R { start :: Int, end :: Int} deriving (Show, Eq, Ord)

parseInput :: String -> [Block]
parseInput = map (parseBlock . map (map read . splitOn ",") . splitOn "~")  . lines
    where
        parseBlock [[x,y,z],[x',y',z']] = Block (R x x') (R y y') (R z z')

inCollision :: Block -> Block -> Bool
inCollision (Block x y z) (Block x' y' z') = x `inRange` x' && y `inRange` y' && z `inRange` z'
    where
        inRange (R s e) (R s' e') = s <= s' && e >= s'  || s' <= s && e' >= s

fallOne :: Block -> Block
fallOne (Block x y z) = Block x y (fall z)
    where fall (R s e) = R (s-1) (e-1)

falling :: [Block] -> Block -> (Block, [Block])
falling onGround block =
    let block' = fallOne block
        (onSameLevel,rest) = span (\b -> (end.z) b == (start.z) block') onGround
        supportedBy = filter (inCollision block') onSameLevel
    in if any (inCollision block') onSameLevel || (start.z) block' == 0
    then (block, supportedBy) else falling rest block'

-- settle :: [Block] -> [(Block, [Block])] 
settle (b:toProcess) onGround =
    let (b', supportedBy) = falling onGround b
    in (b', supportedBy) : settle toProcess (sortOn ((*(-1)).end.z) (b':onGround))
settle [] _ = []

-- part1 blocks = map fst $  filter canDisintegrate settled
part1 blocks = length $ filter canDisintegrate settled
    where
        sortedBlocks = sortOn (start.z) blocks
        settled = settle sortedBlocks []
        supportedBy = M.fromList settled
        supports = foldl (\mem (b,sp) -> aux mem (b,sp)) M.empty settled
        canDisintegrate (b, _) =
            let su = M.findWithDefault [] b supports
            in all (\b' -> length (supportedBy M.! b') > 1 ) su
        aux mem (b, sp) = foldl (\mem' b' -> M.insert b' (b : M.findWithDefault [] b' mem') mem') mem sp

part2 = undefined

main :: IO ()
main = do
    input <- readFile "day22/example.txt"
    let parsed = parseInput input
    print "Day 22"
    -- print parsed
    print $ "Part 1: " ++ (show $ part1 parsed)
    -- print $ "Part 2: " ++ (show $ part2 parsed)

