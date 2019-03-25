type Stack a = [a]

push :: a -> Stack a -> Stack a
push x xs = x:xs

pop :: Stack a -> Stack a
pop (x:xs) = xs

empty :: Stack a -> Bool
empty [] = True
empty (x:xs) = False

top :: Stack a -> a
top (x:xs) = x

f :: [Char] -> Bool
supF :: Stack Char -> [Char] -> Bool
f [] = True
f xs = supF [] xs

supF [] [] = True
supF stack [] = False

supF stack (x:xs) | x == '(' || x == '{' || x == '['                   = supF (push x stack) xs
                  | x == ')' && (not)(empty stack) && top stack == '(' = supF (pop stack) xs
                  | x == ']' && (not)(empty stack) && top stack == '[' = supF (pop stack) xs
                  | x == '}' && (not)(empty stack) && top stack == '{' = supF (pop stack) xs
                  | otherwise = False
