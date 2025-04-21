import SetV1
import Queue
import Stack

losQuePertenecen :: Eq a => [a] -> Set a -> [a]
losQuePertenecen [] s = []
losQuePertenecen (x:xs) s = if belongs x s
                            then x : losQuePertenecen xs s
                            else losQuePertenecen xs s


sinRepetidos :: Eq a => [a] -> [a]
sinRepetidos [] = []
sinRepetidos (x:xs) = if pertenece x xs
                      then sinRepetidos xs 
                      else x : sinRepetidos xs 
                       

pertenece :: Eq a => a -> [a] -> Bool
pertenece _ [] = False
pertenece x (y:ys) = x == y || pertenece x ys

data Tree a = EmptyT | NodeT a (Tree a) (Tree a)

unirTodos :: Eq a => Tree (Set a) -> Set a
unirTodos EmptyT = S [] 0
unirTodos (NodeT s t1 t2) = unionS s (unionS (unirTodos t1) (unirTodos t2))

