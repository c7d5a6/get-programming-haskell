myReverse :: [a] -> [a]
myReverse []     = []
myReverse (x:xs) = myReverse xs ++ [x]


fastFib :: Int -> Int -> Int -> Int
fastFib x y 0   = x
fastFib x y 1   = y
fastFib x y num = fastFib y (x + y) (num - 1)


fib :: Int -> Int
fib x = fastFib 1 1 x
