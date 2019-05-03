find :: [Integer] -> Integer -> Integer

find [] _ = -1
find xs a = if a == head xs then 0 else prev (tail xs) a

prev :: [Integer] -> Integer -> Integer
prev xs a = 1 + (find xs a) 
