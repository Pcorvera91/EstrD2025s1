module PriorityQueue
  (PriorityQueue, emptyPQ, isEmptyPQ, insertPQ, findMinPQ, deleteMinPQ)
 where

data PriorityQueue a = PQ [a]

emptyPQ     :: PriorityQueue a
  -- PROP: describe una priority queue vacía

isEmptyPQ   :: PriorityQueue a -> Bool
  -- PROP: indica si la priority queue dada está vacía

insertPQ    :: Ord a => a -> PriorityQueue a -> PriorityQueue a
  -- PROP: inserta un elemento en la priority queue, según su prioridad

findMinPQ   :: Ord a => PriorityQueue a -> a
  -- PROP: devuelve el elemento con más prioridad (el mínimo) 
  --       de la priority queue
  -- PRECOND: parcial en caso de priority queue vacía

deleteMinPQ :: Ord a => PriorityQueue a -> PriorityQueue a
  -- PROP: devuelve la priority queue sin el elemento con 
  --       más prioridad (el mínimo)
  -- PRECOND: parcial en caso de priority queue vacía



emptyPQ             = PQ []                                        -- O(1)
isEmptyPQ (PQ xs)   = null xs                                      -- O(1)
insertPQ x (PQ xs)  = PQ (x:xs)                                    -- O(1)
findMinPQ (PQ xs)   = minimum xs        -- PARCIAL si xs es vacía  -- O(n)
deleteMinPQ (PQ xs) = PQ (borrarMin xs) -- PARCIAL si xs es vacía  -- O(n)

-- O(n)
borrarMin :: Ord a => [a] -> [a]
  -- PRECOND: la lista no es vacía
borrarMin xs = borrar (minimum xs) xs

-- O(n)
borrar :: Eq a => a -> [a] -> [a]
borrar x []     = []
borrar x (y:ys) = if x==y then ys else y : borrar x ys
   -- OJO. PENSAR bien qué pasa si borro a uno de la misma edad...