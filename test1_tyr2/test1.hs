
---------------------------------------------2------------------------------------
square :: Int -> IO()

square n = helper 1 n where
           helper i n | i == 1    = putStrLn (replicate (n) '*') >> 
                                      helper (i + 1) n
                      | i == n    = putStrLn (replicate (n) '*')
                      | otherwise = putStr("*") >> 
                                      putStr(replicate (n - 2) ' ') >>
                                      putStrLn ("*")  >> 
                                      helper (i + 1) n



---------------------------------------------4----------------------------------

data Auto = Light String String String Int| Heavy Int String Int

sumPrice :: [Auto] -> Int
price :: Auto -> Int

sumPrice [] = 0
sumPrice (x:xs) = price(x) + sumPrice xs
price (Light _ _ _ x) = x
price (Heavy _ _ x) = x

markAuto :: [Auto] -> [String]
mark :: Auto -> String

markAuto [] = []
markAuto (x:xs) = mark(x) : markAuto(xs)
mark (Light _ _ x _) = x
mark (Heavy _ x _) = x
