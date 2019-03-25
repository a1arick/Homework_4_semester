func :: [Integer] -> [Integer]
func l = filter (\x -> cont x) l

cont :: Integer -> Bool
cont x | x == 0 = True
       | x `mod` 10 == 1 || x `mod` 10 == 7 ||  x `mod` 10 == 9 = cont (x `div` 10) 
       | otherwise = False 
