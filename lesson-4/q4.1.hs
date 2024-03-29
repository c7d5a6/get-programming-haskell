import           Data.List

names = [("Ian", "Curtis"),
         ("Bernard","Sumner"),
         ("Peter", "Hook"),
         ("Ann", "Hook"),
         ("Stephen","Morris")]

noNegativeDiff :: Int -> Int -> Int
noNegativeDiff x y = if diff < 0
		     then 0
		     else diff
  where diff = x - y

compareLastNames name1 name2 = if compareLastNames /= EQ
			       then compareLastNames
                               else compareFirstNames
  where compareLastNames =  compare lastName1 lastName2
          where lastName1 = snd name1
                lastName2 = snd name2
	compareFirstNames = compare firstName1 firstName2
	  where	firstName1 = fst name1
        	firstName2 = fst name2


sfOffice name = if lastName < "L"
                then nameText
                     ++ " - PO Box 1234 - San Francisco, CA, 94111"
                else nameText
                     ++ " - PO Box 1010 - San Francisco, CA, 94109"
  where lastName = snd name
        nameText = (fst name) ++ " " ++ lastName


nyOffice name = nameText ++ ": PO Box 789 - New York, NY, 10013"
  where nameText = (fst name) ++ " " ++ (snd name)


renoOffice name = nameText ++ " - PO Box 456 - Reno, NV 89523"
  where nameText = snd name


washingtonOffice name = nameText ++ " - Washington, DC"
  where nameText = (fst name) ++ " " ++ (snd name) ++ " Esq."


getLocationFunction location = case location of
                               "ny" -> nyOffice
                               "sf" -> sfOffice
                               "reno" -> renoOffice
                               "wd" -> washingtonOffice
                               _ -> (\name -> (fst name) ++ " " ++ (snd name))


addressLetter name location = locationFunction name
  where locationFunction = getLocationFunction location
