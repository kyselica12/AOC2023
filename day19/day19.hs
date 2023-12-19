module Main where 

import Data.List.Split (splitOn)
import Data.List
import Text.Parsec 
import Control.Applicative

dataParser = do
    -- a <- sepBy1 workflow (char '\n')  
    a <- many1 (workflow <* (string "\n"))
    string "\n"
    -- b <- many1 (many1 (noneOf "\n") <* (char '\n'))
    b <- sepBy (part) (string "\n")
    eof
    return (a,b) 

workflow :: Parsec String () (String, [Rule], String)
workflow = do 
    name <- many1 letter
    char '{'
    rules <- manyTill (rule) (try (lookAhead (many1 letter <* string "}")))
    x <- many1 letter
    char '}'
    return (name, rules, x)

-- type Rule = Char
-- type Rule = (String, Char, String, String)
data Rule = Rule { f :: Part -> Bool, dst :: String } 
rule = do
    i <- many1 letter 
    sign <- oneOf "<>="
    val <- many1 digit
    char ':'
    dst <- many1 (noneOf ",}")
    return (Rule (\p -> (f1 sign) ((f2 i) p) (read val)) dst)
    where 
        f1 sign = case sign of
            '<' -> (<)
            '>' -> (>)
        f2 i = case i of
            "x" -> x
            "m" -> m
            "a" -> a
            "s" -> s

data Part = Part { x :: Int, m :: Int, a :: Int, s :: Int } deriving (Show)    

part :: Parsec String () Part
part = do
    char '{'
    x <- read <$> (string "x=" *> many1 digit)
    char ','
    m <- read <$> (string "m=" *> many1 digit)
    char ','
    a <- read <$> (string "a=" *> many1 digit)
    char ','
    s <- read <$> (string "s=" *> many1 digit)
    char '}'
    return (Part x m a s)

parseInput input = either (error.show) id (parse dataParser "ERROR" input)

part1 = undefined

part2 = undefined


main :: IO ()
main = do
    input <- parseInput <$> readFile "day19/example.txt"
    print "Day 19"
    -- print $ "Part 1: " ++ (show $ part1 input)
    -- print $ "Part 2: " ++ (show $ part2 input)

