
func :: Num a => a -> [a] -> [a]
func1 :: Num a => a -> [a] -> [a]
func2 :: Num a => a -> [a] -> [a]
func3 :: Num a => a -> [a] -> [a]
func4 :: Num a => a -> [a] -> [a]

func x l = map (\y -> y*x) l
{-Apply reduction to this function:-}
func1 x = map (\y -> y*x)
{-Apply reduction to this function:-}
func2 x = map (*x)
{-Now apply the composition between map and *:-}
func3 x = (map . (*)) x
{-Apply reduction to this function:-}
func4 = map . (*)


main = print (func4 5 [1..10]) 
