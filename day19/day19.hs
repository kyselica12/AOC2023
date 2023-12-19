module Main (main) where

import Data.List
import Text.Parsec
import qualified Data.Map as M

data Rule = Rule { p :: String, sign :: Char, val :: Int, dst :: String} deriving (Show)
data Part = Part { x :: Int, m :: Int, a :: Int, s :: Int } deriving (Show)
data Range = R { l :: Int, r :: Int } | Nil deriving (Show, Eq)
data MultiRange = ItemRange { xr :: Range, mr :: Range, ar :: Range, sr :: Range } deriving (Show)

eol = char '\n'
name = many1 letter
number = read <$> many1 digit
operator = oneOf "<>"
assignInt = count 2 anyToken *> number

dataParser :: Parsec String () ([(String, [Rule], String)], [Part])
dataParser = (,) <$> (sepEndBy workflow eol <* eol) <*> (sepBy part eol <* eof)

workflow :: Parsec String () (String, [Rule], String)
workflow = (,,) <$> (name <* char '{') <*> sepEndBy (try rule) (char ',') <*> name <* char '}'

rule :: Parsec String () Rule
rule = Rule <$> name <*> operator <*> number <*> (char ':' *> name)

part :: Parsec String () Part
part = (\[x,m,a,s] -> Part x m a s) <$> between (char '{') (char '}') (sepBy assignInt (char ','))

parseInput :: String -> ([(String, [Rule], String)], [Part])
parseInput input = either (error.show) id (parse dataParser "ERROR" input)

getOp :: Char -> (Int -> Int -> Bool)
getOp x = if x == '<' then (<) else (>)

applyWorkflow :: Part -> ([Rule], String) -> String
applyWorkflow _ ([], def) = def
applyWorkflow it@(Part x m a s) (r:rs, def) = if getOp (sign r) g (val r) then dst r else applyWorkflow it (rs, def)
    where g = case p r of
            "x" -> x
            "m" -> m
            "a" -> a
            "s" -> s

part1 :: ([(String, [Rule], String)], [Part]) -> Int
part1 (workflows, items) = sum $ map  (aux "in") items
    where
        mem = M.fromList $ map (\(name, rules, dst) -> (name, (rules, dst))) workflows
        aux w it = case w of
            "A" -> x it + m it + a it + s it
            "R" -> 0
            _ ->  aux (applyWorkflow it (mem M.! w)) it

applyRuleItemRange :: Rule -> MultiRange -> (MultiRange, MultiRange)
applyRuleItemRange rule@(Rule p s v d) ir =
    case p of
        "x" -> let (r1,r2) = apllyRuleRange rule p (xr ir) in (ir { xr = r1 }, ir { xr = r2 })
        "m" -> let (r1,r2) = apllyRuleRange rule p (mr ir) in (ir { mr = r1 }, ir { mr = r2 })
        "a" -> let (r1,r2) = apllyRuleRange rule p (ar ir) in (ir { ar = r1 }, ir { ar = r2 })
        "s" -> let (r1,r2) = apllyRuleRange rule p (sr ir) in (ir { sr = r1 }, ir { sr = r2 })

apllyRuleRange :: Rule -> String -> Range -> (Range, Range)
apllyRuleRange (Rule p s v d) x r@(R a b) = if p /= x then (Nil, r) else
    case  (getOp s a v, getOp s b v) of
        (True, True) -> (r, Nil)
        (False, False) -> (Nil, r)
        (True, False) -> (R a (v-1), R v b)
        (False, True) -> (R (v+1) b, R a v)

isNil :: MultiRange -> Bool
isNil (ItemRange x m a s) = x == Nil && m == Nil && a == Nil && s == Nil

rangeSize :: Range -> Int
rangeSize (R a b) = b - a +1

itemRangeCount :: MultiRange -> Int
itemRangeCount (ItemRange x m a s) = product $ map rangeSize [x,m,a,s]

part2 (workflows, _) = sum $ map itemRangeCount $ applyRange "in" (ItemRange (R 1 4000) (R 1 4000) (R 1 4000) (R 1 4000))
    where
        mem = M.fromList $ map (\(name, rules, dst) -> (name, (rules, dst))) workflows
        aux ([],d) range = [(d, range)]
        aux (rule:rs,d) range =
            let (r1,r2) = applyRuleItemRange rule range in
            if isNil range then [] else  (dst rule, r1) : aux (rs,d) r2
        applyRange "A" r = [r]
        applyRange "R" r = []
        applyRange w range = concatMap (uncurry applyRange) (aux (mem M.! w) range)

main :: IO ()
main = do
    input <- parseInput <$> readFile "day19/input.txt"
    print "Day 19"
    print $ "Part 1: " ++ show (part1 input)
    print $ "Part 2: " ++ show (part2 input)