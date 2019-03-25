degree2 :: Int -> [Int]
degree2 x = supList x
  where supList 0 = [1]
        supList n = $ 2 * head prev : prev
          where prev = supList (n - 1) 
