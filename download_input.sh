#!/bin/bash

session=$(cat session.cookie)


curl "https://adventofcode.com/2023/day/$1/input" \
  -H "cookie: session=$session" \
  > "$2/input.txt"
