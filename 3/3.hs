fact :: [Integer] -> Integer
fact l = fun 0 0 0 l l

fun :: Integer -> Integer -> Integer -> Integer -> [Integer] -> [Integer]
fun pos maxSum temp [] l = pos
fun pos maxSum temp tempL l | temp + (head templ) > maxSum = fun (length l - length templ) (temp + (head tempL)) (head templ) (tail templ) l
                            | otherwise = fun pos maxSum (head templ) (tail templ) l    


main = print (fact [1, 5, 6, 2]) 
