module Map2L
    (Map, emptyM, assocM, lookupM, deleteM, domM) 
  where

data Map k v = M [k] [v]
  {- INV.REP.: en M kvs, no hay claves repetidas en kvs -}

emptyM  :: Map k v
  -- PROP.: describe el map vacío

assocM  :: Ord k => k -> v -> Map k v -> Map k v
  -- PROP.: describe el map dado donde la clave dada se asocia
  --        al valor dado (si estaba asociada a otra cosa, solamente
  --        vale la última asociación)

lookupM :: Ord k => k -> Map k v -> Maybe v
  -- PROP.: describe el valor asociado a la clave en el map 
  --        si existe, o Nothing, si no

deleteM :: Ord k => k -> Map k v -> Map k v
  -- PROP.: describe el map dado, p(ero donde la clave dada no 
  --        se asocia a ningún valor

domM    :: Ord k => Map k v -> [k]
  -- PROP.: describe la lista de todas las claves definidas
  --        en el map, sin repetidos (el dominio del map)

emptyM               = M [] []                  
assocM k v (M ks vs) = M (k : ks) (v : vs)
lookupM k  (M ks vs) = buscar k ks vs
deleteM k  (M ks vs) = elimineishon k ks vs     
domM       (M ks vs) = ks         

buscar :: Eq k => k -> [k] -> [v] -> Maybe v
buscar _ [] [] = Nothing
buscar k (kp:ks) (v:vs) = if k == kp
                             then Just v
                             else buscar k ks vs

elimineishon :: Eq k => k -> ks -> vs -> Map k v
elimineishon _ [] [] = emptyM
elimineishon k (kp:ks) (v:vs) = if k == kp
                                then elimineishon k ks vs
                                else assocM kp v (elimineishon k ks vs)

