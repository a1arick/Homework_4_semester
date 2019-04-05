merge :: [a] -> [a] -> [a]
merge xs     []     = xs
merge []     ys     = ys
merge (x:xs) (y:ys) = x : y : merge xs ys


listOdd = [ -1 | x <- [1..], x `mod` 2 == 0]
listEven = [ 1 | x <- [1..], x `mod` 2 == 1]


f:: [Integer]
f = zipWith (*) (merge listEven listOdd) [1..]

main = print (f) 