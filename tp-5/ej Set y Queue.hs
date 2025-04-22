import SetV1
import Queue



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
unirTodos EmptyT = emptyS
unirTodos (NodeT s t1 t2) = unionS s (unionS (unirTodos t1) (unirTodos t2))

lengthQ :: Queue a -> Int
lengthQ q = if isEmptyQ q
            then 0
            else 1 + lengthQ (dequeue q)


queueToList :: Queue a -> [a]
queueToList emptyQ = []
queueToList q = (firstQ q) : (queueToList (dequeue q)) 

unionQ :: Queue a -> Queue a -> Queue a
unionQ q1 q2 = if isEmptyQ q1
               then q2
               else Q (queueToList q1 ++ queueToList q2)







