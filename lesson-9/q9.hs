import           Data.Char

harmonic :: Int -> Double
harmonic n = foldl (+) 0.0 (map (\x -> 1/x) (take n [1.0,2.0..]))


isPalindrome :: String -> Bool
isPalindrome line = text == reverse text
	where text = filter (\x -> x /= ' ') (map (\x -> toLower x) line)
