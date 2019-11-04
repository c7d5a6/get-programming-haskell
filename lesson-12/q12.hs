type FirstName = String
type LastName = String
type MiddleName = String
type Age = Int
type Height = Int

data Name = Name FirstName LastName
	  | NameWithMiddle FirstName MiddleName LastName
data Sex = Male | Female
data RhType = Pos | Neg
data ABOType = A | B | AB | O
data BloodType = BloodType ABOType RhType
data Patient = Patient { name      :: Name
		       , sex       :: Sex
		       , age       :: Int
		       , height    :: Int
		       , weight    :: Int
		       , bloodType :: BloodType }

showName :: Name -> String
showName (Name f l)             = f ++ " " ++ l
showName (NameWithMiddle f m l) = f ++ " " ++ m ++ " " ++ l

showPatientName :: Name -> String
showPatientName (Name f l)             = l ++ ", " ++ f
showPatientName (NameWithMiddle f m l) = l ++ ", " ++ f ++ " " ++ m

showSex :: Sex -> String
showSex Male   = "Male"
showSex Female = "Female"

sexInitial :: Sex -> Char
sexInitial Male   = 'M'
sexInitial Female = 'F'

showRh :: RhType -> String
showRh Pos = "+"
showRh Neg = "-"
showABO :: ABOType -> String
showABO A  = "A"
showABO B  = "B"
showABO AB = "AB"
showABO O  = "O"
showBloodType :: BloodType -> String
showBloodType (BloodType abo rh) = showABO abo ++ showRh rh

canDonateTo :: BloodType -> BloodType -> Bool
canDonateTo (BloodType O _) _               = True
canDonateTo _ (BloodType AB _)              = True
canDonateTo (BloodType A _) (BloodType A _) = True
canDonateTo (BloodType B _) (BloodType B _) = True
canDonateTo _ _                             = False

patientInfo :: Patient -> String
patientInfo patient = dotLine ++ "\n" ++
                      "Patient Name: " ++ printedName ++ "\n" ++
		      "Sex: " ++ showSex (sex patient) ++ "\n" ++
                      "Age: " ++ show (age patient) ++ "\n" ++
                      "Height: " ++ show (height patient) ++ " in.\n" ++
                      "Weight: " ++ show (weight patient) ++ " lbs.\n" ++
                      "BloodType: " ++ showBloodType (bloodType patient) ++ "\n" ++
                      dotLine ++ "\n"
			      where dotLine = take (nameLength ) (cycle "*")
				    printedName = showPatientName (name patient)
				    nameLength = length printedName + 14


jackieSmith :: Patient
jackieSmith = Patient {name = Name "Jackie" "Smith"
		      , age = 43
		      , sex = Female
		      , height = 62
		      , weight = 115
		      , bloodType = BloodType O Neg }
