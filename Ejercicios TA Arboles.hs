data Usuario = U String [Recurso]
data Recurso = Energia Int | Datos Int deriving Show
data NodoVirtual = Servidor Usuario NodoVirtual NodoVirtual | NodoVacio
-- 1. usuariosConDatos :: NodoVirtual -> [Usuario]
-- Lista los usuarios que poseen al menos un recurso de tipo datos.

usuariosConDatos :: NodoVirtual -> [Usuario]
usuariosConDatos (Servidor u nv1 nv2) = if hayDatos (recursosDe u)  
                                        then u : usuariosConDatos nv1 ++ usuariosConDatos nv2
                                        else usuariosConDatos nv1 ++ usuariosConDatos nv2
usuariosConDatos NodoVacio = []

recursosDe :: Usuario -> [Recurso]
recursosDe (U _ rs) = rs 

hayDatos :: [Recurso] -> Bool 
hayDatos [] = False
hayDatos (r:rs) = esDato r || hayDatos rs

esDato :: Recurso -> Bool
esDato (Datos n) = True
esDato _ = False



-- 2. energiaTotalServidor :: Usuario -> NodoVirtual -> Int
-- Calcula la cantidad total de energía disponible para un servidor dado.

energiaTotalServidor :: Usuario -> NodoVirtual -> Int
energiaTotalServidor user (Servidor u nv1 nv2) = if esElUsuario user u 
                                                 then energiaTotalDe u + energiaTotalServidor user nv1 + energiaTotalServidor user nv2
                                                 else energiaTotalServidor user nv1 + energiaTotalServidor  user nv2
energiaTotalServidor user NodoVacio = 0


esElUsuario :: Usuario -> Usuario -> Bool 
esElUsuario u1 u2 = nombre u1 == nombre u2 

nombre :: Usuario -> String 
nombre (U n _) = n

energiaTotalDe :: Usuario -> Int 
energiaTotalDe u = energiaTotal (recursosDe u)

energiaTotal :: [Recurso] -> Int 
energiaTotal [] = 0
energiaTotal (r:rs) = if not (esDato r) 
                      then valorDeEnergia r + energiaTotal rs 
                      else energiaTotal rs 

valorDeEnergia :: Recurso -> Int 
valorDeEnergia (Energia n) = n 
valorDeEnergia _ = 0




-- 3. servidorConMasRecursos :: NodoVirtual -> (Usuario, Int)
-- Indica el servidor con la mayor suma total de recursos.

servidorConMasRecursos :: NodoVirtual -> (Usuario, Int)
--PREC:Hay al menos 1 usuario en el Nodo Virtual
servidorConMasRecursos (Servidor u nv1 nv2) = elegirEntre (usuarioConCantDeRecursos u) (elegirEntre((servidorConMasRecursos nv1) (servidorConMasRecursos nv2)))
servidorConMasRecursos NodoVacio = error "No hay usuarios"

usuarioConCantDeRecursos :: Usuario -> (Usuario,Int)
usuarioConCantDeRecursos u = (u,cantidadDeRecursos (recursosDe u))

cantidadDeRecursos :: [Recurso] -> Int 
cantidadDeRecursos rs = length rs

elegirEntre :: (Usuario,Int) -> (Usuario,Int) -> (Usuario,Int)
elegirEntre u1 u2 = if segundo u1 > segundo u2 
                    then u1 
                    else u2 


segundo :: (a,b) -> b 
segundo (x,y) =y

data Dir = Izq | Der
data Objeto = Tesoro | Chatarra
data Cofre = Cofre [Objeto]
data Mapa = Fin Cofre | Bifurcacion Cofre Mapa Mapa

hayTesoro :: Mapa -> Bool
--Indica si hay un tesoro en alguna parte del mapa
hayTesoro (Fin c) = hayTesoroC c
hayTesoro (Bifurcacion c m1 m2) = hayTesoroC c || hayTesoro m1 || hayTesoro m2 

hayTesoroC :: Cofre -> Bool 
hayTesoroC c = hayTesoroO (objetos c)

hayTesoroO :: [Objeto] -> Bool
hayTesoroO [] = False 
hayTesoroO (o:os) = esTesoro o || hayTesoroO os 

esTesoro :: Objeto -> Bool 
esTesoro Tesoro = True 
esTesoro _ = False 

objetos :: Cofre -> [Objeto]
objetos (Cofre os) = os 

hayTesoroEn :: [Dir] -> Mapa -> Bool
-- Indica si al final del camino hay un tesoro. Nota: el final de un camino se representa con una lista vacía de direcciones.
hayTesoroEn [] (Fin c) = hayTesoroC c
hayTesoroEn (d:ds) (Bifurcacion c m1 m2) = if esIzq d
                                           then hayTesoroEn ds m1 
                                           else hayTesoroEn ds m2 

esIzq :: Dir -> Bool 
esIzq Izq = True 
esIzq _ = False

caminoAlTesoro :: Mapa -> [Dir]
-- Indica el camino al tesoro. Precondición: existe un tesoro y es único
caminoAlTesoro (Fin c) = []
caminoAlTesoro (Bifurcacion c m1 m2) = if hayTesoro m1 
                                       then Izq : caminoAlTesoro m1 
                                       else Der : caminoAlTesoro m2

caminoDeLaRamaMasLarga :: Mapa -> [Dir]
-- Indica el camino de la rama más larga.
caminoDeLaRamaMasLarga (Fin c) = []
caminoDeLaRamaMasLarga (Bifurcacion c m1 m2) = if tamanio m1 > tamanio m2 
                                               then Izq : caminoDeLaRamaMasLarga m1 
                                               else Der : caminoDeLaRamaMasLarga m2 



tamanio :: Mapa -> Int
tamanio (Fin c) = 0
tamanio (Bifurcacion c m1 m2) = 1 + maxDelPar (tamanio m1,tamanio m2)

maxDelPar :: (Int, Int) -> Int
maxDelPar (n, m) = if n > m then n else m

tesorosPorNivel :: Mapa -> [[Objeto]]
tesorosPorNivel (Fin c) = [tesoros (objetos c)]
tesorosPorNivel (Bifurcacion c m1 m2) = [tesoros (objetos c)] ++ agruparElementos (tesorosPorNivel m1) (tesorosPorNivel m2)

agruparElementos :: [[a]] -> [[a]] -> [[a]]
agruparElementos [] ys = ys
agruparElementos xs [] = xs
agruparElementos (x:xs) (y:ys) = (x ++ y) : agruparElementos xs ys

tesoros :: [Objeto] -> [Objeto]
tesoros [] = []
tesoros (o:os) = if esTesoro o 
                 then o : tesoros os 
                 else tesoros os


data Componente = LanzaTorpedos | Motor Int | Almacen [Barril]
data Barril = Comida | Oxigeno | Torpedo | Combustible
data Sector = S SectorId [Componente] [Tripulante]
type SectorId = String
type Tripulante = String
data Tree a = EmptyT | NodeT a (Tree a) (Tree a)
data Nave = N (Tree Sector)

sectores :: Nave -> [SectorId]
-- Propósito: Devuelve todos los sectores de la nave
sectores (N ts) = sectoresT ts 

sectoresT :: Tree Sector -> [SectorId]
sectoresT EmptyT = []
sectoresT (NodeT s ti td) = sid s : sectoresT ti ++ sectoresT td


sid :: Sector -> SectorId
sid (S sID _ _) = sID 

poderDePropulsion :: Nave -> Int
-- Propósito: Devuelve la suma de poder de propulsión de todos los motores de la nave. Nota:
-- el poder de propulsión es el número que acompaña al constructor de motores.
poderDePropulsion (N ts) = poderT ts 

poderT :: Tree Sector -> Int 
poderT EmptyT = 0
poderT (NodeT s t1 t2) = totalDePotencia (componentes s) + poderT t1 + poderT t2 

totalDePotencia :: [Componente] -> Int 
totalDePotencia [] = 0
totalDePotencia (c:cs) = if esMotor c 
                         then potenciaDe c + totalDePotencia cs 
                         else totalDePotencia cs 

esMotor :: Componente -> Bool 
esMotor c = case c of 
    Motor _ -> True 
    _       -> False

potenciaDe :: Componente -> Int 
potenciaDe c = case c of 
    Motor n -> n 
    _       -> 0 

barriles :: Nave -> [Barril]
barriles (N ts) = barrilesT ts 

barrilesT :: Tree Sector -> [Barril]
barrilesT EmptyT = []
barrilesT (NodeT s t1 t2) = totalDeBarriles(componentes s) ++ barrilesT t1 ++ barrilesT t2

totalDeBarriles :: [Componente] -> [Barril]
totalDeBarriles [] = []
totalDeBarriles (c:cs) = if esAlmacen c 
                         then barrilesDe c ++ totalDeBarriles cs 
                         else totalDeBarriles cs 

esAlmacen :: Componente -> Bool 
esAlmacen c = case c of
    Almacen _ -> True 
    _         -> False

barrilesDe :: Componente -> [Barril]
barrilesDe c = case c of 
    Almacen bs -> bs 
    _          -> []

componentes :: Sector -> [Componente]
componentes (S _ cs _) = cs

agregarASector :: [Componente] -> SectorId -> Nave -> Nave
agregarASector cs sid (N ts) = (N agregarASectorT cs sid ts)

agregarASectorT :: [Componente] -> SectorId -> Tree Sector -> Tree Sector 
agregarASectorT cs sid EmptyT = 
agregarASectorT cs sid (NodeT s ti td) = s agregarASectorT cs sid ti agregarASectorT cs sid td 

data RAList a = MkR Int (Map Int a) (Heap a)

{-INV.REP:
Sea MkR n ma ha
*los valores que aparecen en ma también aparecen en ha
*la cantidad de pares k v de ma == datos de ha y es igual a n - 1
* N == H == K

-}
emptyRAL :: RAList a
emptyRAL = MkR 0 emptyM emptyH

isEmptyRAL :: RAList a -> Bool
isEmptyRAL (MkR _ _ ha) = isEmptyH ha

lengthRAL :: RAList a -> Int
lengthRAL (MkR n _ _) = n-1

get :: Int -> RAList a -> a
get i (MkR _ ma _ ha) = fromJust(lookupM ma i) 

minRAL :: Ord a => RAList a -> a
minRAL (MkR _ _ ha) = findMin ha

add :: Ord a => a -> RAList a -> RAList a
add x (MkR n ma ha) = MkR (n+1) (assocM n x ma) (insertH  x ha)

elems :: Ord a => RAList a -> [a]
elems (MkR _ ma _) = valoresALista (domM ma ) ma 

valoresALista :: Ord a => [Int] -> Map Int a -> [a]
valoresALista [] m = [] 
valoresALista (n:ns) m = case lookupM m n of 
                            Nothing -> valoresALista ns m 
                            Just x  -> x : valoresALista ns m 



remove :: Ord a => RAList a -> RAList a
-- Precondición: la lista no está vacía.
remove (MkR n ma ha) = case lookupM n ma of
                           Nothing -> error "La lista no debe estar vacía"
                           Just v  -> MkR (n-1) (deleteM n ma) (eliminarDelHeap v ha)

--verifico elemento en el map,y luego con ese valor lo busco en la Heap y lo elimino


eliminarDelHeap :: Ord a => Int -> Heap a -> Heap a 
eliminarDelHeap n h = if n == (findMinH h)
                          then deleteMinH h
                          else insertH (findMinH h) (eliminarDelHeap n (deleteMinH h))

-- Si el elemento buscado era el mínimo (el primero de la h) lo elimino y ya.
-- Sino caso recursivo. Cómo? Saco el mínimo y lo inserto en la heap que tiene el elemento buscado eliminado y sin el elemento mínimo ya visto.

set :: Ord a => Int -> a -> RAList a -> RAList a
-- Propósito: reemplaza el elemento en la posición dada.
-- Precondición: el índice debe existir.
set pos x (MkR n ma ha) = case lookupM pos ma of 
                            Nothing -> error "La posicion no existe"
                            Just v  -> MkR n (reemplazarM pos x ma) (reemplazarEnHeap x ha)

reemplazarM :: Int -> a -> Map Int a -> Map Int a 
reemplazarM i x m =  assocM i x (deleteM i m)

reemplazarEnHeap :: Ord a => a -> a -> Heap a -> Heap a
reemplazarEnHeap viejo nuevo h = insertH nuevo (quitarElemento viejo h)

quitarElemento :: Ord a => a -> Heap a -> Heap a
quitarElemento x h = if isEmptyH h
                        then emptyH
                        else if x == findMin h
                                then deleteMin h
                                else insertH (findMin h) (quitarElemento x (deleteMin h))

addAt :: Ord a => Int -> a -> RAList a -> RAList a
-- Propósito: agrega un elemento en la posición dada.
-- Precondición: el índice debe estar entre 0 y la longitud de la lista.
-- Observación: cada elemento en una posición posterior a la dada pasa a estar en su posición siguiente.
-- Eficiencia: O(N log N).
-- Sugerencia: definir una subtarea que corra los elementos del Map en una posición a partir de una posición dada. Pasar también como argumento la máxima posición posible.

addAt i x (MkR n ma ha) = MkR (n+1) (dezplazarM (domM ma) (assocM i x ma)) (insertH x ha)