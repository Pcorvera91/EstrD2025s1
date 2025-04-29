import Map


valuesM :: Eq k => Map k v -> [Maybe v]
--Propósito: obtiene los valores asociados a cada clave del map.
valuesM m = todosLosValoresDe (keys m) m

todosLosValoresDe :: Eq a => [k] -> Map k v -> [Maybe v]
todosLosValoresDe [] m = []
todosLosValoresDe (k:ks) m = lookupM k m : todosLosValoresDe ks m

mergeMaps :: Eq k => Map k v -> Map k v -> Map k v
mergeMaps m1 m2 = juntar (keys m1) m1 m2

juntar :: Eq k => [k] -> Map k v -> Map k v -> Map k v 
juntar [] m1 m2 = m2
juntar (k:ks) m1 m2 = assocM k (fromJust(lookupM k m1 )(juntar ks m1 m2))

todasAsociadas :: Eq k => [k] -> Map k v -> Bool
todasAsociadas [] m = True
todasAsociadas (k:ks) m = esJust (lookupM k m) && todasAsociadas ks m


esJust :: a -> Bool
esJust (Just _) = True
esJust Nothing = False

listToMap :: Eq k => [(k, v)] -> Map k v
listToMap [] = emptyM
listToMap ((k,v) : kvs) = assocM k v (listToMap kvs)

mapToList :: Eq k => Map k v -> [(k, v)]
mapToList m = formarPar (domM m) (valuesM m)

formarPar :: [k] -> [Maybe v] -> [(k,v)]
formarPar [] vs = []
formarPar ks [] = []
formarPar (k:ks) (Just v:vs) = (k,v) : formarPar ks vs

agruparEq :: Eq k => [(k, v)] -> Map k [v]
agruparEq [] = emptyM
agruparEq ((k,v):kvs) =
  let m = agruparEq kvs  -- Primero armo el Map con el resto
  in case lookupM k m of
       Nothing -> assocM k [v] m
       Just vs -> assocM k (v:vs) m

incrementar :: Eq k => [k] -> Map k Int -> Map k Int
incrementar [] m = m
incrementar (k:ks) m =
  case lookupM k m of
    Just n  -> incrementar ks (assocM k (n + 1) m)
    Nothing -> incrementar ks m



