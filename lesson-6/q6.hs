repeatNew :: a -> [a]
repeatNew x = cycle (x:[])

subseq :: Int -> Int -> [a] -> [a]
subseq start end list = take (end - start + 1) (drop start list)

--inFirstHalf :: a -> [a] -> Eq a
inFirstHalf val list = elem val firstHalf
	where firstHalf = take half list
       	      half = div (length list) 2
