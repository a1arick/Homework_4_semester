find :: [Integer] -> Integer -> Integer

find [] _ = -1
find xs a = if a == head xs then 0 else prev (tail xs) a

prev :: [Integer] -> Integer -> Integer
prev xs a = 1 + (find xs a) 

main = print (find [3,4,5,7,2,4,2,4,23,3] 2) 