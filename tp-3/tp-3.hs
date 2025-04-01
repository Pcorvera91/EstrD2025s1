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
poner colorAPoner CeldaVacia = Bolita colorAPoner CeldaVacia
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
hayTesoroEn 0 Fin = False
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
cantTesorosEntre desde hasta Fin = 
cantTesorosEntre desde hasta (Cofre obs c) =
cantTesorosEntre desde hasta (Nada c) =




