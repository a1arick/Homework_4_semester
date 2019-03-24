
func :: Num a => a -> [a] -> [a]
func1 :: Num a => a -> [a] -> [a]
func2 :: Num a => a -> [a] -> [a]
func3 :: Num a => a -> [a] -> [a]
func4 :: Num a => a -> [a] -> [a]

func x l = map (\y -> y*x) l
{-Применим к этой функции эта-редукцию:-}
func1 x = map (\y -> y*x)
{-Применим к этой функции эта-редукцию:-}
func2 x = map (*x)
{-Теперь применим композицию между map и *:-}
func3 x = (map . (*)) x
{-Применим к этой функции эта-редукцию:-}
func4 = map . (*)


main = print (func4 5 [1..10]) 