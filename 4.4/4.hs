
data Poly = P [Int]

instance Show Poly where
  show (P [])          = " "
  show(P (x:xs))       | (n == 0)  = (if x > 0 then "+" else "") ++ (show x)
                       | (x == 0)  = show(P xs)
                       | (n == 1)  = (if x > 0 then "+" else "") ++ (show x) ++ "x" ++ show(P xs)
                       | (x == 1)  = "+x^" ++ (show n) ++ show(P xs)
                       | (x == -1) = "-x^" ++ (show n) ++ show(P xs)
                       | otherwise = (if x > 0 then "+" else "") ++ (show x) ++ "x^" ++ (show n) ++ show(P xs)
                         where n = length xs 

sumPoly :: Poly -> Poly -> Poly 
sumPoly (P xs) (P ys) = P (zipWith (+) (balanceHelper xs ys) (balanceHelper ys xs))

balanceHelper :: [Int] -> [Int] -> [Int]
balanceHelper xs ys = balancePoly xs (max (length xs) (length ys) )


balancePoly :: [Int] -> Int -> [Int]
balancePoly xs maxLength | (length xs < maxLength) = balancePoly (0:xs) maxLength
                         | (length xs == maxLength) = xs

multiplyByConst :: [Int] -> Int -> Poly
multiplyByConst xs a = P (map (a*) xs)

multiplyByX :: [Int] -> Poly
multiplyByX xs = P (xs ++ [0])

multiplyPoly :: Poly -> Poly -> Poly 
multiplyPoly (P(xs)) (P ys) = multiplyPoly' (P(reverse xs)) (P ys)

multiplyPoly' :: Poly -> Poly -> Poly 
multiplyPoly' (P []) _ = P []
multiplyPoly' (P(x:xs)) (P ys) = sumPoly  (multiplyByConst ys x)  ( multiplyPoly' (P xs) (multiplyByX ys))

-- example input: P [1,2,3]
-- example input: sumPoly (P [1,2,3]) (P [1,2,3,4,5]) 
-- example input: multiplyPoly (P [1,2,3]) (P [1,2,3,4,5])  