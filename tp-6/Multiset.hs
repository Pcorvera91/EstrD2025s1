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