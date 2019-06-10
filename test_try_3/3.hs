import System.Random

f :: [Int] -> IO([Int])
f [] = return []
f (x : xs) = do 
	randomValue <- getStdRandom (randomR(1, 100))
	randomList <- f xs
	return (randomValue : randomList)