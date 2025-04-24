import Map

valuesM :: Eq k => Map k v -> [Maybe v]
--Propósito: obtiene los valores asociados a cada clave del map.
valuesM m = todosLosValoresDe (keys m) m

todosLosValoresDe :: Eq a => [k] -> Map k v -> [Maybe v]
todosLosValoresDe [] m = []
todosLosValoresDe (k:ks) m = lookupM k m : todosLosValoresDe ks m

mergeMaps :: Eq k => Map k v -> Map k v -> Map k v
mergeMaps m1 m2 = juntar (keys m1) m1 m2

juntar :: Eq k => [k] => Map k v => Map k v => Map k v 
juntar [] m1 m2 = m2
juntar (k:ks) m1 m2 = assocM k (fromJust(lookupM k m1 )(juntar ks m1 m2))

