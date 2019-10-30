inc :: Int -> Int
inc n = n + 1

double :: Int -> Int
double n = n*2

square :: Int -> Int
square n = n^2

ifEven :: (Int -> Int) -> Int -> Int
ifEven f x = if even x
		then f x
                else x

ifEvenInc :: Int -> Int
ifEvenInc = ifEven inc

ifEvenDouble :: Int -> Int
ifEvenDouble = ifEven double

ifEvenSquare :: Int -> Int
ifEvenSquare = ifEven square

binaryPartialApplication :: (a -> b -> c) -> a -> (b -> c)
binaryPartialApplication f x = (\y -> f x y)

ifEvenIncV2 :: Int -> Int
ifEvenIncV2 = binaryPartialApplication ifEven inc


