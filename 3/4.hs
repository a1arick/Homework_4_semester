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
f str = supF [] str

supF [] [] = True
supF stack [] = False

supF stack str | (head str) == '(' || head str == '{' || head str == '['     = supF (push (head str) stack) (tail str)
               | (head str) == ')' && (not)(empty stack) && top stack == '(' = supF (pop stack) (tail str)
               | (head str) == ']' && (not)(empty stack) && top stack == '[' = supF (pop stack) (tail str)
               | (head str) == '}' && (not)(empty stack) && top stack == '{' = supF (pop stack) (tail str)
               | otherwise = False