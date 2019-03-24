
reverseList :: [a] -> [a]
reverseList list = supReverse list []

supReverse :: [a] -> [a] -> [a]
supReverse [] temp = temp
supReverse xs temp = supReverse (tail xs) (head xs:temp)

main = print (reverseList [3,4,5,7,2,4,2,4,23,3]) 