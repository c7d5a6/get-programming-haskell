myTail :: [a] -> [a]
myTail []     = []
myTail (_:xs) = xs

myGCD :: Int -> Int -> Int
myGCD a 0 = a
myGCD a b = myGCD b (mod a b)
