-- cup :: a -> a
cup fl0z = \message -> message fl0z
get0z aCup = aCup (\fl0z -> fl0z)
drink aCup ozDrank = if ozDrank >= 0
		       then cup ozDiff
		       else cup 0
	where fl0z = get0z aCup
	      ozDiff = fl0z - ozDrank
isEmpty aCup = get0z aCup == 0

coffeeCup = cup 12

