f :: (a -> Bool) -> [a] -> Bool
f condition xs = foldr (&&) True (map condition xs)
