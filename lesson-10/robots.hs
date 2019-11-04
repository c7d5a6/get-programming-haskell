robot (name, attack, hp) = \message -> message (name, attack, hp)
name (n,_,_) = n
attack (_,a,_) = a
hp (_,_,hp) = hp
getName aRobot = aRobot name
setName aRobot newName = aRobot (\(n,a,h) -> (newName,a,h))
getAttack aRobot = aRobot attack
setAttack aRobot newAttack = aRobot (\(n,a,h) -> robot (n,newAttack,h))
getHP aRobot = aRobot hp
setHP aRobot newHP = aRobot (\(n,a,h) -> robot (n,a,newHP))

printRobot aRobot = aRobot (\(n,a,h) -> n ++
					" attack:" ++ (show a) ++
					" hp:" ++ (show h))

damage aRobot attackDamage = aRobot (\(n,a,h) ->
				        robot (n,a,h-attackDamage))

fight aRobot defender = damage defender attack
	where attack = if getHP aRobot > 10
			  then getAttack aRobot
			  else 0

robotLifes list = map (\(n,a,h) -> show h) list

killerRobot = robot ("Kill3r",25,200)
nicerRobot = setName killerRobot "kitty"
gentlerRobot = setAttack killerRobot 5
softerRobot = setHP killerRobot 50
fastRobot = robot ("speedy", 15, 40)
slowRobot = robot ("slowpoke",20,30)

--robotList = gentlerRobot:nicerRobot:[]
