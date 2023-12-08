module Main where
import Data.List
import qualified Data.Map as M
import Text.Parsec

inputParser :: Parsec String () (String,  M.Map String (String, String))
inputParser = (\x y -> (x, M.fromList y)) <$> (many1 (letter <|> digit) <* string "\n\n") <*> (sepBy1 parseNode (string "\n") <* eof)

parseNode :: Parsec String () (String, (String, String))
parseNode = (,) <$> many1 (letter <|> digit) <*>  (string " = " *> parseTuple)

parseTuple :: Parsec String () (String, String)
parseTuple = (\[x,y]->(x,y)) <$> (string "(" *> sepBy1 (many1 (letter <|> digit)) (string ", ")) <* string ")"

infiniteMoves :: String -> String
infiniteMoves moves = moves ++ infiniteMoves moves

move :: M.Map String (String, String) -> String -> String -> Int
move _ _ [_,_,'Z'] = 0
move g (m:ms) node = let f = if m == 'L' then fst else snd in 1 + move g ms (f $ g M.! node)

countMoves :: String -> M.Map String (String, String) -> [String] -> [Int]
countMoves moves g = map (move g (infiniteMoves moves) )

part1 :: (String , M.Map String (String, String)) -> Int
part1 (moves,g) = move g (infiniteMoves moves) "AAA"

part2 :: (String , M.Map String (String, String)) -> Int
part2 (moves, g) = (foldl1 lcm . countMoves moves g . filter (\x -> last x == 'A' ))  (M.keys g)

main :: IO ()
main = do
    parsed <- parse inputParser "PARSE ERROR" <$> readFile "day08/input.txt"
    let input = either (error . show) id parsed
    print $ "Part 1: " ++ show (part1 input)
    print $ "Part 2: " ++ show (part2 input)