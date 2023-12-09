import argparse
import os


parser = argparse.ArgumentParser(description='Create a template for a new problem')

parser.add_argument("idx",type=int, help="Number of the day")

args = parser.parse_args()

template =\
f"""module Main where 

import Data.List.Split (splitOn)
import Data.List 

parseInput = undefined

part1 = undefined

part2 = undefined


main :: IO ()
main = do
    input <- readFile "input.txt"
    let parsed = parseInput input
    print "Day {args.idx:02}"
    -- print $ \"Part 1: \" ++ (show $ part1 parsed)
    -- print $ \"Part 2: \" ++ (show $ part2 parsed)

"""

dir = f"day{args.idx:02}"
os.makedirs(dir, exist_ok=True)

with open(f"{dir}/day{args.idx:02}.hs", "w") as f:
    f.write(template)

with open(f"{dir}/example.txt", "w") as f:
    f.write("")

with open(f"{dir}/input.txt", "w") as f:
    f.write("")