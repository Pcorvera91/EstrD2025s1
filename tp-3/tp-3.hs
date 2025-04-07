
data Color = Azul | Rojo
data Celda = Bolita Color Celda | CeldaVacia

nroBolitas _ CeldaVacia = 0
nroBolitas colorBuscado (Bolita colorCelda cel) =
  es1SiSino0 (elColorBuscadoEsIgualAlDeLaCelda colorBuscado colorCelda) + nroBolitas colorBuscado cel

es1SiSino0 :: Bool -> Int
es1SiSino0 True  = 1
es1SiSino0 False = 0

elColorBuscadoEsIgualAlDeLaCelda :: Color -> Color -> Bool
elColorBuscadoEsIgualAlDeLaCelda Azul Azul = True
elColorBuscadoEsIgualAlDeLaCelda Rojo Rojo = True
elColorBuscadoEsIgualAlDeLaCelda _ _ = False

poner :: Color -> Celda -> Celda
poner colorAPoner c = Bolita colorAPoner c

sacar :: Color -> Celda -> Celda
sacar colorASacar CeldaVacia = CeldaVacia
sacar colorASacar (Bolita col c) = if (elColorBuscadoEsIgualAlDeLaCelda colorASacar col)
                                   then c
                                   else Bolita col (sacar colorASacar c)


ponerN :: Int -> Color -> Celda -> Celda
ponerN 0 col cel =  cel
ponerN n col cel = poner col (ponerN  (n-1) col cel) 

data Objeto = Cacharro | Tesoro
data Camino = Fin | Cofre [Objeto] Camino | Nada Camino

hayTesoro :: Camino -> Bool
hayTesoro Fin = False
hayTesoro (Cofre obs c) = hayAlgunTesoroEn obs || hayTesoro c
hayTesoro (Nada c) =  hayTesoro c

hayAlgunTesoroEn :: [Objeto] -> Bool
hayAlgunTesoroEn [] = False
hayAlgunTesoroEn (o:os) =  esTesoro o || hayAlgunTesoroEn os

esTesoro :: Objeto -> Bool
esTesoro Tesoro = True
esTesoro Cacharro = False

pasosHastaTesoro :: Camino -> Int
pasosHastaTesoro Fin = 0
pasosHastaTesoro (Cofre obs c) = 0
pasosHastaTesoro (Nada c) = 1 + pasosHastaTesoro c

hayTesoroEn :: Int -> Camino -> Bool
hayTesoroEn 0 (Cofre obs c) = hayAlgunTesoroEn obs 
hayTesoroEn 0 (Nada c) = False
hayTesoroEn n Fin = False
hayTesoroEn n (Cofre obs c) = hayTesoroEn (n-1) c
hayTesoroEn n (Nada c)= hayTesoroEn (n-1) c

alMenosNTesoros :: Int -> Camino -> Bool
alMenosNTesoros n c = cantidadDeTesoros c >= n

cantidadDeTesoros :: Camino -> Int
cantidadDeTesoros Fin = 0
cantidadDeTesoros (Cofre obs c) = totalDeTesoros obs + cantidadDeTesoros c
cantidadDeTesoros (Nada c) = cantidadDeTesoros c

totalDeTesoros :: [Objeto] -> Int
totalDeTesoros [] = 0
totalDeTesoros (o:os) = es1SiSino0 (esTesoro o) + totalDeTesoros os


cantTesorosEntre :: Int -> Int -> Camino -> Int
cantTesorosEntre desde hasta c = avanzarNYContarTesoros desde (hasta - desde) c

avanzarNYContarTesoros :: Int -> Camino -> Int
avanzarNYContarTesoros 0 (Cofre obs c) = totalDeTesoros obs
avanzarNYContarTesoros 0 (Nada c) = 0
avanzarNYContarTesoros n Fin = 0
avanzarNYContarTesoros n (Cofre obs c) = totalDeTesoros obs + avanzarNYContarTesoros (n-1)
avanzarNYContarTesoros n (Nada c) = avanzarNYContarTesoros (n-1) c

data Tree a = EmptyT | NodeT a (Tree a) (Tree a)


-- f:: Tree a -> b
-- f EmptyT = ...
-- f (NodeT x t1 t2) = ... x ... f t1 ... ft2 ..

sumarT :: Tree Int -> Int
sumarT EmptyT = 0
sumarT (NodeT n ti td) = n + sumarT ti + sumarT td

sizeT :: Tree a -> Int
sizeT EmptyT = 0
sizeT (NodeT x ti td) = 1 + sizeT ti + sizeT td

mapDobleT :: Tree Int -> Tree Int
mapDobleT EmptyT = EmptyT
mapDobleT (NodeT n ti td) = NodeT (elDoble n) (mapDobleT ti) (mapDobleT td)

elDoble :: Int -> Int
elDoble n = n*2

perteneceT :: Eq a => a -> Tree a -> Bool
perteneceT x EmptyT = False
perteneceT x (NodeT y ti td) = x == y || perteneceT x ti || perteneceT x td

aparicionesT :: Eq a => a -> Tree a -> Int
aparicionesT e EmptyT = 0
aparicionesT e (NodeT x ti td) = es1SiSino0 (e == x) + aparicionesT e ti + aparicionesT e td

leaves :: Tree a -> [a]
leaves EmptyT = []
leaves (NodeT x EmptyT EmptyT) = [x]
leaves (NodeT x ti td) = leaves ti ++ leaves td 

esHoja :: Tree a -> Bool
esHoja (NodeT x EmptyT EmptyT) =True
esHoja _ = False

heightT :: Tree a -> Int
heightT EmptyT = 0
heightT (NodeT x EmptyT EmptyT) = 1
heightT (NodeT x ti td) = 1 + maxDelPar ((heightT ti),(heightT td))

maxDelPar :: (Int, Int) -> Int
maxDelPar (n, m) = if n > m then n else m

mirrorT :: Tree a -> Tree a
mirrorT EmptyT = EmptyT
mirrorT (NodeT x ti td) = NodeT x (mirrorT td) (mirrorT ti)

toList :: Tree a -> [a]
toList EmptyT = []
toList (NodeT x ti td) = toList ti ++ [x] ++ toList td


levelN :: Int -> Tree a -> [a]
levelN n EmptyT = []
levelN 0 (NodeT x ti td) = [x]
levelN n (NodeT x ti td) = levelN (n-1) ti ++ levelN (n-1) td

listPerLevel :: Tree a -> [[a]]
listPerLevel EmptyT = []
listPerLevel (NodeT x ti td) = [[x]] ++ agruparElementos (listPerLevel ti) (listPerLevel td)

agruparElementos :: [[a]] -> [[a]] -> [[a]]
agruparElementos [] ys = ys
agruparElementos xs [] = xs
agruparElementos (x:xs) (y:ys) = (x ++ y) : agruparElementos xs ys

ramaMasLarga :: Tree a -> [a]
ramaMasLarga EmptyT = []
ramaMasLarga (NodeT x ti td) = if (heightT ti) > (heightT td)
                               then x : ramaMasLarga ti
                               else x : ramaMasLarga td

todosLosCaminos :: Tree a -> [[a]]

data ExpA = Valor Int 
  | Sum ExpA ExpA
  | Prod ExpA ExpA
  | Neg ExpA

eval :: ExpA -> Int
eval (Valor n) = n
eval (Sum e1  e2) = e1 + e2
eval (Prod e1 e2) = e1 * e2
eval (Neg e) = (-1) * e1

simplificar :: ExpA -> ExpA
simplificar (Valor n) = Valor n
simplificar (Sum e1 e2) = Valor simplificar e1 + Valor simplificar e2
simplificar (Sum (Valor 0) e2) = simplificar e2
simplificar (Sum e1 (Valor 0)) = simplificar e1
simplificar (Prod e1 e2) = Valor simplificar e1 * Valor simplificar e2 
simplificar (Prod (Valor 0) e2) = Valor 0
simplificar (Prod e1 (Valor 0)) = Valor 0
simplificar (Neg e) = Neg simplificar e
simplificar (Neg (Neg e)) = simplificar e


