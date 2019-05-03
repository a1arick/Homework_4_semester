
fib :: Integer -> Integer
fib' :: Integer -> Integer -> Integer -> Integer
fib n | n < 0  = fib' n (-1) 0
      | n == 0 = 0
      | n > 0  = fib' n 1 0

fib' n x y | n < 0  = fib' (n+1) (x+y) x
           | n == 0 = y
           | n > 0  = fib' (n-1) (x+y) x

