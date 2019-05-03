maxSumIndex :: [Integer] -> Integer
supFunc :: Integer -> Integer -> Integer -> [Integer] -> Integer
sumTempAndNext :: [Integer] -> Integer
maxSumIndex xs = supFunc 0 0 0 xs

supFunc maxPos maxSum i xs | xs == []                    = maxPos
                           | sumTempAndNext xs <= maxSum = supFunc maxPos maxSum (i + 1) (tail xs)
                           | otherwise                   = supFunc (i + 1) (sumTempAndNext xs) (i + 1) (tail xs)

sumTempAndNext (x:xs) = if length (x:xs) == 1 then x else x + head xs
