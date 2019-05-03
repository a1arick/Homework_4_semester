
reverseList :: [a] -> [a]
reverseList list = supReverse list []

supReverse :: [a] -> [a] -> [a]
supReverse [] temp = temp
supReverse xs temp = supReverse (tail xs) (head xs:temp)
