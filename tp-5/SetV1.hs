module SetV1
  (Set , emptyS, addS,belongs,sizeS,removeS,unionS,setToList)
  where

data Set a = S [a] Int

emptyS :: Set a
addS :: Eq a => a -> Set a -> Set a
pertenece :: Eq a => a -> [a] -> Bool
belongs :: Eq a => a -> Set a -> Bool
sizeS :: Eq a => Set a -> Int
removeS :: Eq a => a -> Set a -> Set a
unionS :: Eq a => Set a -> Set a -> Set a
setToList :: Eq a => Set a -> [a]



-- Crea un conjunto vacío

emptyS = S [] 0

-- Agrega un elemento al conjunto si no está

addS x (S xs n)  = if pertenece x xs
                then S xs n
                else S (x : xs) (n+1)

-- Auxiliar: verifica si un elemento pertenece a una lista

pertenece _ [] = False
pertenece x (y:ys) = x == y || pertenece x ys


belongs x (S xs n) = pertenece x xs


sizeS (S _ n) = n


removeS y (S (x:xs) n) = 
  if y == x
    then S xs (n-1)
    else rearmar x (removeS y (S xs n))

rearmar :: a -> Set a -> Set a
rearmar x (S xs n) = S (x:xs) n

-- Función auxiliar para agregar al inicio (manteniendo la estructura Set)

agregarAlInicio :: a -> Set a -> Set a
agregarAlInicio x (S xs n)  = S (x:xs) (n+1)


unionS (S [] _) s2 = s2
unionS (S (x:xs) n) s2 = unionS (S xs (n - 1)) (addS x s2)


setToList (S xs n) = xs


