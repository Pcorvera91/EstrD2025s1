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
  if hayTesoroEnLaListaDeObjetos (objetos c)
    then []
    else if (hayTesoro m1) 
    then Izq : caminoAlTesoro m1
    else Der : caminoAlTesoro m2 


caminoDeLaRamaMasLarga :: Mapa -> [Dir]
caminoDeLaRamaMasLarga (Fin c) = []
caminoDeLaRamaMasLarga (Bifurcacion c m1 m2) = if (profundidad m1) > (profundidad m2)
                                               then Izq : caminoDeLaRamaMasLarga m1
                                               else Der : caminoDeLaRamaMasLarga m2

profundidad :: Mapa -> Int
profundidad (Fin c) = 1
profundidad (Bifurcacion c m1 m2) = 1 + max (profundidad m1) (profundidad m2)


tesorosPorNivel :: Mapa -> [[Objeto]]
tesorosPorNivel (Fin c) = [soloTesoros (objetos c)]
tesorosPorNivel (Bifurcacion c m1 m2) =
  [soloTesoros (objetos c)] ++ agruparElementos (tesorosPorNivel m1) (tesorosPorNivel m2)

soloTesoros :: [Objeto] -> [Objeto]
soloTesoros [] = []
soloTesoros (o:os) = if esTesoro o
                     then o : soloTesoros os
                     else soloTesoros os

agruparElementos :: [[a]] -> [[a]] -> [[a]]
agruparElementos [] ys = ys
agruparElementos xs [] = xs
agruparElementos (x:xs) (y:ys) = (x ++ y) : agruparElementos xs ys


todosLosCaminos :: Mapa -> [[Dir]]
todosLosCaminos (Fin _) = []
todosLosCaminos (Bifurcacion _ mi md) =
  ([[]] ++ agregaATodos Izq (todosLosCaminos mi))
  ++ agregaATodos Der (todosLosCaminos md)

agregaATodos :: Dir -> [[Dir]] -> [[Dir]]
agregaATodos _ [] = []
agregaATodos d (ds:dss) = (d:ds) : agregaATodos d dss

apariciones :: Eq a => [a] -> [(a,Int)]
apariciones  [] = []
apariciones  (x:xs) = (x, 1 + contarVeces x xs) : apariciones xs

contarVeces :: Eq  a => a -> [a] -> Int
contarVeces x [] = 0
contarVeces x (y:ys) = if x==y then 1 + contarVeces x ys else contarVeces x ys

data Componente = LanzaTorpedos | Motor Int | Almacen [Barril]
data Barril = Comida | Oxigeno | Torpedo | Combustible
data Sector = S SectorId [Componente] [Tripulante]
type SectorId = String
type Tripulante = String
data Tree a = EmptyT | NodeT a (Tree a) (Tree a)
data Nave = N (Tree Sector)


sectores :: Nave -> [ SectorId ]
sectores (N t) = sectoresT t

sectoresT :: Tree Sector -> [ SectorId ]
sectoresT EmptyT          = []
sectoresT (NodeT s t1 t2) = idSector s : sectoresT t1 
                                      ++ sectoresT t2

idSector (S id _ _) = id

poderDePropulsion :: Nave -> Int
poderDePropulsion (N t) = propulsionT t

propulsionT :: Tree Sector -> Int
propulsionT EmptyT          = 0
propulsionT (NodeT s t1 t2) = propulsionS s + propulsionT t1 + propulsionT t2

propulsionS :: Sector -> Int
propulsionS (S _ cs _) = propulsionCs cs

propulsionCs :: [Componente] -> Int
propulsionCs []     = 0
propulsionCs (c:cs) = propulsionC c + propulsionCs cs

propulsionC :: Componente -> Int
propulsionC (Motor n) = n
propulsionC _         = 0

barriles :: Nave -> [Barril]
barriles (N t) = barrilesT t

barrilesT :: Tree Sector -> [Barril]
barrilesT EmptyT = []
barrilesT (NodeT s t1 t2) = barrilesDelSector s : barrilesT t1 ++ barrilesT t2

barrilesDelSector :: [Componente] -> [Barril]
barrilesDelSector (S _ comps _) = soloBarriles comps

soloBarriles :: [Componente] -> [Barril]
soloBarriles [] = []
soloBarriles (cp:cps) = extraerBarriles cp ++ soloBarriles cps

extraerBarriles :: Componente -> [Barril]
extraerBarriles (Almacen bs) = bs
extraerBarriles _ = []

agregarASector :: [Componente] -> SectorId -> Nave -> Nave
agregarASector cps id (N t) = (N buscarSectorYAgregar cps id t)

buscarSectorYAgregar :: [Componente] -> SectorId -> Tree Sector -> Tree Sector
buscarSectorYAgregar cps id EmptyT = EmptyT
buscarSectorYAgregar cps id (NodeT s t1 t2) = NodeT ( agregarCompSiCorresponde cps id s) (buscarSectorYAgregar cps id t1) (buscarSectorYAgregar cps id t2) 

agregarComponentesSiCorresponde :: [Componente] -> SectorId -> Sector -> Sector
agregarComponentesSiCorresponde cps id (S sid comps trips) =
  if sid == id
    then S sid (comps ++ cps) trips
    else S sid comps trips




asignarTripulanteA :: Tripulante -> [SectorId] -> Nave -> Nave
asignarTripulanteA tr ids (N t) = (N asignacionASectores tr ids t)

asignacionASectores :: Tripulante -> [SectorId] -> Tree Sector -> Tree Sector
asignacionASectores tr ids EmptyT = EmptyT
asignacionASectores tr ids (NodeT s t1 t2) = if (pertenece (sectorId s) ids)
                                            then NodeT (asignacionDeTripulanteAlSector tr s) (asignacionASectores tr ids t1) (asignacionASectores tr ids t2)
                                            else NodeT s (asignacionASectores tr ids t1) (asignacionASectores tr ids t2)

asignacionDeTripulanteAlSector :: Tripulante -> Sector -> Sector
asignacionDeTripulanteAlSector tr s  = S id comps (tr : trip)

pertenece :: (Eq a) => a -> [a] -> Bool
pertenece x [] = False
pertenece x (y : ys) = (x == y) || pertenece x ys

sectorId :: Sector -> String
sectorId (S id _ _) = id


sectoresAsignados :: Tripulante -> Nave -> [SectorId]

type Presa = String -- nombre de presa
type Territorio = String -- nombre de territorio
type Nombre = String -- nombre de lobo
data Lobo = Cazador Nombre [Presa] Lobo Lobo Lobo
  | Explorador Nombre [Territorio] Lobo Lobo
  | Cría Nombre
data Manada = M Lobo


buenaCaza :: Manada -> Bool
buenaCaza m = cantidadDeAlimento m > cantidadDeCrias m

cantidadDeAlimento :: Manada -> Int
cantidadDeAlimento (M lobo) = cantidadDeAlimentoL lobo

cantidadDeAlimentoL :: Lobo -> Int
cantidadDeAlimentoL (Cazador _ presas l1 l2 l3) = alimentoEn presas
                                                + cantidadDeAlimentoL l1
                                                + cantidadDeAlimentoL l2
                                                + cantidadDeAlimentoL l3
cantidadDeAlimentoL (Explorador _ _ l1 l2)      = cantidadDeAlimentoL l1
                                                + cantidadDeAlimentoL l2
cantidadDeAlimentoL (Cria _)                    = 0

alimentoEn :: [Presa] -> Int
alimentoEn ps = length ps

elAlfa :: Manada -> (Nombre, Int)
elAlfa (M lobo) = elAlfaL lobo

elAlfaL :: Lobo -> (Nombre, Int)
elAlfaL (Cazador nom presas l1 l2 l3) = elegirEntre (nom, alimentoEn presas)
                                                    (elegirEntre (elAlfaL l1)
                                                                 (elegirEntre (elAlfaL l2)
                                                                              (elAlfaL l3)))
elAlfaL (Explorador nom _ l1 l2)      = elegirEntre (elAlfaL l1)
                                                    (elegirEntre (elAlfaL l2)
                                                                 (nom, 0))
elAlfaL (Cria nom)                    = (nom, 0)


elegirEntre :: (Nombre, Int) -> (Nombre, Int) -> (Nombre, Int)
elegirEntre (nom1, c1) (nom2, c2) = if (c1>=c2) then (nom1, c1)
                                                else (nom2, c2)

