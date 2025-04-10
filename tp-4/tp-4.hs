data Pizza = Prepizza
    | Capa Ingrediente Pizza
data Ingrediente = Salsa
    | Queso
    | Jamon
    | Aceitunas Int

cantidadDeCapas :: Pizza -> Int
cantidadDeCapas Prepizza = 0
cantidadDeCapas (Capa i p) = 1 + cantidadDeCapas p

armarPizza :: [Ingrediente] -> Pizza
armarPizza [] = Prepizza
armarPizza (i:is) = Capa i (armarPizza is)

sacarJamon :: Pizza -> Pizza
sacarJamon Prepizza = Prepizza
sacarJamon (Capa i p) = Capa (reemplazarJamonPorQueso i) (sacarJamon p)

reemplazarJamonPorQueso :: Ingrediente -> Ingrediente
reemplazarJamonPorQueso Jamon = Queso
reemplazarJamonPorQueso i = i

tieneSoloSalsaYQueso :: Pizza -> Bool
tieneSoloSalsaYQueso Prepizza = True
tieneSoloSalsaYQueso (Capa i p) = esQuesoOSalsa i && tieneSoloSalsaYQueso p

esQuesoOSalsa :: Ingrediente -> Bool
esQuesoOSalsa Queso = True
esQuesoOSalsa Salsa = True
esQuesoOSalsa _ = False

duplicarAceitunas :: Pizza -> Pizza
duplicarAceitunas Prepizza = Prepizza
duplicarAceitunas (Capa i p) = Capa (duplicarSiEsAceituna i) (duplicarAceitunas p)

duplicarSiEsAceituna :: Ingrediente -> Ingrediente
duplicarSiEsAceituna (Aceitunas n) = Aceitunas (n * 2)
duplicarSiEsAceituna i = i

cantCapasPorPizza :: [Pizza] -> [(Int, Pizza)]
cantCapasPorPizza [] = [] 
cantCapasPorPizza (p:ps) = (cantidadDeCapas p , p) :  cantCapasPorPizza ps




data Dir = Izq | Der
data Objeto = Tesoro | Chatarra
data Cofre = Cofre [Objeto]
data Mapa = Fin Cofre
    | Bifurcacion Cofre Mapa Mapa


hayTesoroEn :: [Dir] -> Mapa -> Bool
hayTesoroEn [] (Fin c) = hayTesoroEnLaListaDeObjetos (objetos c)
hayTesoroEn (d:ds) (Fin c) = False
hayTesoroEn [] (Bifurcacion c m1 m2) = hayTesoroEnLaListaDeObjetos (objetos c)
hayTesoroEn (d:ds) (Bifurcacion c m1 m2) = if esIzq d then hayTesoroEn ds m1 else hayTesoroEn ds m2

hayTesoroEnLaListaDeObjetos :: [Objeto] -> Bool
hayTesoroEnLaListaDeObjetos [] = False
hayTesoroEnLaListaDeObjetos (o:os) = esTesoro o || hayTesoroEnLaListaDeObjetos os

esTesoro :: Objeto -> Bool
esTesoro Tesoro = True
esTesoro Chatarra = False

objetos :: Cofre -> [Objeto]
objetos (Cofre os) = os

esIzq :: Dir -> Bool
esIzq Izq = True
esIzq Der = False 

hayTesoro :: Mapa -> Bool
hayTesoro (Fin c) = hayTesoroEnLaListaDeObjetos (objetos c)
hayTesoro (Bifurcacion c m1 m2) = hayTesoroEnLaListaDeObjetos (objetos c) || hayTesoro m1 || hayTesoro m2


caminoAlTesoro :: Mapa -> [Dir]
caminoAlTesoro (Fin c) = []
caminoAlTesoro (Bifurcacion c m1 m2) =
  if perteneceTesoro (objetosDeCofre c)
    then []
    else if (hayTesoro m) 
    then Izq : caminoAlTesoro m1
    else Der : caminoAlTesoro m2 



