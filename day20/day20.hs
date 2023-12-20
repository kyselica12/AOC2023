module Main (main) where

import Data.List.Split (splitOn)
import Data.List
import Text.Parsec hiding (State)
import Debug.Trace (trace)
import qualified Data.Map as M
import Data.Maybe (fromJust)

data State = B | F {on::Bool} | C {mem::M.Map String Bool} deriving (Show, Eq)

data Module = M {name::String, to::[String], s::State} deriving (Show, Eq)
data Signal = L | H deriving (Show, Eq)

-- inputP :: Parsec String () [String]
inputP = do
    lines <- sepEndBy moduleP (char '\n')
    eof
    return lines

moduleP = do
    t <- lookAhead anyToken
    name <- many1 (noneOf " ")
    string " -> "
    x <- many1 (noneOf "\n")
    return $ construcsModule name t x
    where
        getName t name= if t == '%' || t == '&' then tail name else name
        construcsModule name t x =
            let to = splitOn ", " x
            in case t of
                '%' -> M (getName t name) to (F False)
                '&' -> M (getName t name) to (C M.empty)
                _ -> M (getName t name) to B

parseInput input =
    let parsed = either (error.show) id (parse inputP "ERROR" input)
        (cons, others) = partition isCons parsed
        cons' = map (\m@(M n _ s) -> m{s=s{mem= M.fromList (map (\m -> (name m, False)) $ filter (\m -> n `elem` to m ) parsed)}}) cons
        modules = M.fromList $ map (\m -> (name m, m)) $ cons' ++ others
    in  modules
        where
            isCons (M _ _ s) = case s of
                B -> False
                F _ -> False
                C _ -> True

recieveSignal m@(M n to B) from signal = (Just signal, m)
recieveSignal m@(M n to (F on)) from H = (Nothing, m)
recieveSignal m@(M n to (F on)) from L = if on then (Just L, m{s=F False}) else (Just H, m{s=F True})
recieveSignal m@(M n to (C mem)) from signal =
    let mem' = M.insert from (signal == H) mem
    in if and (M.elems mem') then (Just L, m{s=C mem'}) else (Just H, m{s=C mem'})

oneRound modules = aux modules [(L, "INPUT","broadcaster")] 0 0 []
    where
        aux ms ((signal, src, dst):xs) cl ch switched =
            let (signal', m') = recieveSignal (ms M.! dst) src signal
                ms' = M.insert dst m' ms
                next = map (fromJust signal', dst,)  (to m')
                cl' = if signal == L then cl+1 else cl
                ch' = if signal == H then ch+1 else ch
                i' = if dst == "xn" && signal == H then src : switched else switched
            in
            if not (M.member dst ms) then  aux ms xs cl' ch' i'
            else case signal' of
                Nothing -> aux ms' xs cl' ch' i'
                Just signal' -> aux ms' (xs++next) cl' ch' i'
        aux ms [] cl ch i  = (ms,cl, ch,i)

part1 parsed =
    let (_,a,b) = foldl (\(ms,a,b) _ -> let (ms',a',b',_) = oneRound ms in (ms',a+a', b+b')) (parsed,0,0) [1..1000]
    in a*b

part2 parsed = M.foldl lcm 1 $ aux parsed 0 counts
    where
        aux ms i counts =
            let (ms',_,_,x) = oneRound ms
                mem' = mem (s (ms' M.! "xn"))
                counts' = foldl (\m k -> M.insert k (i+1) m) counts x
            in if all (>0) (M.elems counts') then counts' else aux ms' (i+1) counts'
        counts = M.fromList $ map (, 0) $ M.keys ( mem (s (parsed M.! "xn")))

main :: IO ()
main = do
    -- input <- readFile "day20/input.txt"
    input <- readFile "day20/input.txt"
    let parsed = parseInput input
    print "Day 20"
    -- print parsed
    print $ "Part 1: " ++ (show $ part1 parsed)
    print $ "Part 2: " ++ (show $ part2 parsed)