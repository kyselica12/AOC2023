#!/bin/bash

session=$(cat session.cookie)


curl "https://adventofcode.com/2023/day/$1/input" \
  -H "cookie: session=$session" \
  > "$2/input.txt"

val1=8; val2=2
# -H "cookie: session=53616c7465645f5fd38319bdcc594a698d10d3569f54b04cf2c32e253b7a059970ce4f64e3edc5a13ebdd3dcbb9721ca6d98ba4ccec7df245cd37b1bbcc425cf" \