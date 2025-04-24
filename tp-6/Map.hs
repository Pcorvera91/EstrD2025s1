module Map
    (Map, emptyM, assocM, lookupM, deleteM, domM) 
  where

data Map k v = M [(k,v)]
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
  -- PROP.: describe el map dado, pero donde la clave dada no 
  --        se asocia a ningún valor

domM    :: Ord k => Map k v -> [k]
  -- PROP.: describe la lista de todas las claves definidas
  --        en el map, sin repetidos (el dominio del map)

emptyM             = M []                  -- O(1)
assocM k v (M kvs) = M (asociar k v kvs)   -- O(n)
                  -- M ((k,v) : kvs)  -- ASÍ NO, porque puede violar el Invariante!!
lookupM k  (M kvs) = buscar k kvs          -- O(n)
deleteM k  (M kvs) = M (borrar k kvs)      -- O(n)
domM       (M kvs) = claves kvs            -- O(n)

-- O(n)
buscar :: Eq k => k -> [(k,v)] -> Maybe v
buscar k []            = Nothing
buscar k ((k',v'):kvs) = if k==k' then Just v' else buscar k kvs

-- O(n)
claves :: [(k,v)] -> [k]
claves []          = []
claves ((k,v):kvs) = k : claves kvs

-- O(n)
asociar :: Eq k => k -> v -> [(k,v)] -> [(k,v)]
asociar k v []            = [ (k,v) ]
asociar k v ((k',v'):kvs) = if k==k' then (k',v):kvs else (k', v') : asociar k v kvs

-- TRUCHA porque se la pasa pone y saca, pone y saca el constructor M
-- RECOMENDACIÓN: las subtareas NO deberían cambiar de nivel...
asociarMasTrucho :: Eq k => k -> v -> [(k,v)] -> Map k v
asociarMasTrucho k v []            = M [ (k,v) ]
asociarMasTrucho k v ((k',v'):kvs) = 
     if k==k' then M ((k',v):kvs)
              else let M kvs' = asociarMasTrucho k v kvs
                    in M ((k', v') : kvs')

-- O(n)
borrar :: Eq k => k -> [(k,v)] -> [(k,v)]
borrar k []            = []
borrar k ((k',v'):kvs) = if k==k' then kvs else (k',v') : borrar k kvs