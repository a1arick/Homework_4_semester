
func :: Num a => a -> [a] -> [a]
func1 :: Num a => a -> [a] -> [a]
func2 :: Num a => a -> [a] -> [a]
func3 :: Num a => a -> [a] -> [a]
func4 :: Num a => a -> [a] -> [a]

func x l = map (\y -> y*x) l
{-�������� � ���� ������� ���-��������:-}
func1 x = map (\y -> y*x)
{-�������� � ���� ������� ���-��������:-}
func2 x = map (*x)
{-������ �������� ���������� ����� map � *:-}
func3 x = (map . (*)) x
{-�������� � ���� ������� ���-��������:-}
func4 = map . (*)


main = print (func4 5 [1..10]) 