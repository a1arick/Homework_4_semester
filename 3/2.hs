func :: [Integer]

func = 1 : 7 : 9 : concatMap (flip (zipWith (+).repeat.(* 10)) [1, 7, 9]) func
