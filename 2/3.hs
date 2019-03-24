f :: Int -> Int

f x = if x == 0 then 0 else (mod x 10) + (supFunc x)
  where supFunc x = f $ div x 10

main = print (f 35454) 