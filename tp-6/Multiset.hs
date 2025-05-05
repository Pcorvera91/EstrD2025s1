module Multiset
    (Multiset, emptyMS, addMS, ocurrencesMS
             , unionMS, intersectionMS, multiSet2List) 
  where

import Map

data Multiset a = MS (Map a Int)

emptyMS      :: MultiSet a
  -- PROP.: describe el multiset vacío (todos los elementos en 0)

addMS        :: Ord a => a -> MultiSet a -> MultiSet a
  -- PROP.: describe el multiset resultando de agregar el elemento 
  --        al multiset dado, incrementando su cantidad de apariciones

occurencesMS :: Ord a => a -> MultiSet a -> Int
  -- PROP.: describe la cantidad de apariciones del elemento en el multiset

unionMS      :: Ord a => MultiSet a -> MultiSet a -> MultiSet a
  -- PROP.: une dos multisets (las ocurrencias de un elemento son las que aparecen en ambos)

intersectMS  :: Ord a => MultiSet a -> MultiSet a -> MultiSet a
  -- PROP.: interseca dos multisets (las ocurrencias de un elemento son el mínimo entre ambos)

ms2list      :: Multiset a -> [(a,Int)]
  -- PROP.: lista los elementos del multiset, con su número de ocurrencias

emptyMS = MS emptyM
addMs x (MS m) = MS (agregar x m) 

agregar :: Ord a => a -> Map a Int -> Map a Int
agregar x m = case lookupM x m of
                Just n  -> assocM x (n + 1) m
                Nothing -> assocM x 1 m


occurencesMS x (MS m) = apariciones x m

apariciones :: Ord a => a -> Map a Int -> Int
apariciones x m = lookupM x m 


unionMS (MS m1) (MS m2) = MS (unionAux (domM m2) m1 m2)

-- Función auxiliar que recorre las claves de m2 y actualiza m1 con la suma de ocurrencias
unionAux :: Ord a => [a] -> Map a Int -> Map a Int -> Map a Int
unionAux [] m1 _ = m1
unionAux (k:ks) m1 m2 =
  unionAux ks (assocM k (valorEn m1 k + valorEn m2 k) m1) m2

-- Devuelve 0 si no hay valor asociado
valorEn :: Ord a => Map a Int -> a -> Int
valorEn m k = case lookupM k m of
                Nothing -> 0
                Just n  -> n

ms2list (MS m) = multisetALista (domM m) m 

multisetALista :: Ord a => [a] -> Map a Int -> [(a,Int)]
multisetALista [] m = 
multisetALista 





