sumPair :: [Integer] -> [Integer] -> [Integer]
sumPair [] list = list
sumPair list [] = list

sumPair (head1:tail1) (head2:tail2) =  head1 + head2 : sumPair tail1 tail2

sumLists :: [Integer] -> [Integer] -> [Integer] -> [Integer]
sumLists x1 x2 x3 = sumPair x1 (sumPair x2 x3)

main = print (sumLists  [1,2,3] [4,5] [6]) 
