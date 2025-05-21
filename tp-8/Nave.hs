

{-
    INV.REP:
    Todo tripulante presente en el Map Nombre Tripulante también está en la MaxHeap Tripulante.

    Si un tripulante aparece en ambos (Map y Heap), debe ser exactamente el mismo (con mismos sectores asignados y demás atributos).

    Si un tripulante tiene un SectorId asignado, ese sector debe existir en el Map SectorId Sector.

    Todos los nombres de tripulantes listados en cada Sector (en el Set Nombre) deben existir como claves en el Map Nombre Tripulante

    El tipo Sector es un tipo abstracto, y representa al sector de una nave, el cual contiene componentes y tripulantes asignados.


    El tipo Tripulante es un tipo abstracto, y representa a un tripulante dentro de la nave, el cual tiene un nombre, un rango
    y sectores asignados.

    Un sector está vacío cuando no tiene tripulantes, y la nave está vacía si no tiene ningún tripulante.

    Puede haber tripulantes sin sectores asignados.

-}

data Nave = N (Map SectorId Sector) 
              (Map Nombre Tripulante) 
              (MaxHeap Tripulante)


construir :: [SectorId] -> Nave
construir s = N (armarSectores s) emptyM emptyH

armarSectores :: [SectorId] -> Map SectorId Sector
armarSectores [] = emptyM
armarSectores (s:ss) = assocM s (crearS s) (armarSectores ss)


ingresarT :: Nombre -> Rango -> Nave -> Nave
ingresarT n r (N ms mn ht) = N ms (contratarTripM n r mn) (contratarTripH n r ht)

contratarTripM :: Nombre -> Rango -> Map Nombre Tripulante -> Map Nombre Tripulante
contratarTripM n r m = assocM n (crearT n r) m

contratarTripH :: Nombre -> Rango -> MaxHeap Tripulante -> MaxHeap Tripulante 
contratarTripH n r h = insertH (crearT n r)

sectoresAsignados :: Nombre -> Nave -> Set SectorId
sectoresAsignados n (N _ mt _ ) = buscarSectores n mt 

buscarSectores :: Nombre -> Map Nombre Tripulante -> Set SectorId
buscarSectores n m = case lookupM n m of 
                     Just v ->  sectoresT v 
                     Nothing -> error "El tripulante no existe"


datosDeSector :: SectorId -> Nave -> (Set Nombre, [Componente])
-- Propósito: Dado un sector, devuelve los tripulantes y los componentes asignados a ese sector.
-- Precondición: Existe un sector con dicho id.
datosDeSector id (N ms _ _) = dataS id ms 

dataS :: SectorId -> Map SectorId Sector -> (Set Nombre, [Componente])
dataS id m = case lookupM id m of 
             Just v -> (tripulantesS v , componentesS v)
             Nothing -> error "No existe sector con el ID dado"

tripulantesN :: Nave -> [Tripulante]
tripulantesN (N _ _ ht) = todosLosTripulantes ht 

todosLosTripulantes :: MaxHeap Tripulante -> [Tripulante]
todosLosTripulantes ht = if not (isEmptyH ht)
                         then maxH ht : todosLosTripulantes (deleteMax ht)
                         else []


agregarASector :: [Componente] -> SectorId -> Nave -> Nave
-- Funciones a usar: 
-- agregarC 
-- lookupM y assocM 
-- Eficiencia: O(C + log S), siendo C la cantidad de componentes dados.
agregarASector cs id (N ms mt ht) = N (agregarCompsASector cs id ms) mt ht 

agregarComps :: [Componente] -> SectorId -> Map SectorId Sector -> Map SectorId Sector
agregarComps cs id m = case lookupM id m of
                              Just s -> assocM id (agregarCompsASector cs s) m
                              Nothing -> m 

agregarCompsASector :: [Componente] -> Sector -> Sector 
agregarCompsASector [] s = s 
agregarCompsASector (c:cs) s = agregarC c (agregarCompsASector s) 

 asignarASector :: Nombre -> SectorId -> Nave -> Nave
-- Propósito: Asigna un sector a un tripulante.
-- Nota: No importa si el tripulante ya tiene asignado dicho sector.
-- Precondición: El tripulante y el sector existen.
-- Eficiencia: O(log S + log T + T log T)
asignarASector n id (N ms mt ht) = N (asignarTripS n id ms) 
                                     (asignarSectorMT n id mt) 
                                     (asignarSectorHT n id ht) 

asignarTripS :: Nombre -> SectorId -> Map SectorId Sector 
asignarTripS n id m = case lookupM id m of 
                        Just s -> assocM id (agregarT n s) m 
                        Nothing -> error "El sector no existe"

asignarSectorMT :: Nombre -> SectorId -> Map Nombre Tripulante
asignarSectorMT n id m = case lookupM n m of 
                         Just t -> assocM n (asignarS id t) m 
                         Nothing -> error "El tripulante no existe" 

asignarSectorHT :: Nombre -> SectorId -> MaxHeap Tripulante -> MaxHeap Tripulante
asignarSectorHT n id h = reconstruirTripulantes n id (sacarTripulantes n h) (tripulanteObjetivo n h)

-- Recorre y extrae todos los tripulantes distintos a n
sacarTripulantes :: Nombre -> MaxHeap Tripulante -> [Tripulante]
sacarTripulantes n h =
  if isEmptyH h
    then []
    else
      if nombre (maxH h) == n
        then sacarTripulantes n (deleteMax h)
        else maxH h : sacarTripulantes n (deleteMax h)

-- Busca al tripulante con el nombre dado
tripulanteObjetivo :: Nombre -> MaxHeap Tripulante -> Tripulante
tripulanteObjetivo n h =
  if isEmptyH h
    then error "Tripulante no encontrado"
    else
      if nombre (maxH h) == n
        then asignarS id (maxH h)
        else tripulanteObjetivo n (deleteMax h)

-- Inserta el tripulante modificado y los demás en la heap nueva
reconstruirTripulantes :: Nombre -> SectorId -> [Tripulante] -> Tripulante -> MaxHeap Tripulante
reconstruirTripulantes n id [] t = insertH t emptyH
reconstruirTripulantes n id (t1:ts) t = insertH t1 (reconstruirTripulantes n id ts t)


-- USUARIO

sectores :: Nave -> Set SectorId
sectores n = sectoresConTrip (nombreDeTripulantes (tripulantesN n)) n 

nombreDeTripulantes :: [Tripulantes] -> [Nombre]
nombreDeTripulantes [] = []
nombreDeTripulantes (t:ts) = nombre t : nombreDeTripulantes ts

sectoresConTrip :: [Nombre] -> Nave -> Set SectorId
sectoresConTrip [] nave = emptyS 
sectoresConTrip (n:ns) nave = unionS (sectoresAsignados n nave) (sectoresConTrip ns nave)

data Componente = LanzaTorpedos | Motor Int | Almacen [Barril]
data Barril = Comida | Oxigeno | Torpedo | Combustible


sinSectoresAsignados :: Nave ->[Tripulante]

barriles :: Nave -> [Barril]
barriles n = almacenes(todosLosComponentes n)

almacenes :: [Componente] -> [Barril]
almacenes [] = []
almacenes (c:cs) = if esAlmacen c 
                   then barril c ++ almacenes cs 
                   else almacenes cs 

todosLosComponentes :: Nave -> [Componente]
todosLosComponentes n = 



{-

b) construir :: [SectorId] -> Nave
Propósito: Construye una nave con sectores vacíos, en base a una lista de identificadores de sectores.
Eficiencia: O(S)
c) ingresarT :: Nombre -> Rango -> Nave -> Nave
Propósito: Incorpora un tripulante a la nave, sin asignarle un sector.
Eficiencia: O(log T)
d) sectoresAsignados :: Nombre -> Nave -> Set SectorId
Propósito: Devuelve los sectores asignados a un tripulante.
Precondición: Existe un tripulante con dicho nombre.
Eficiencia: O(log M)
e) datosDeSector :: SectorId -> Nave -> (Set Nombre, [Componente])
Propósito: Dado un sector, devuelve los tripulantes y los componentes asignados a ese sector.
Precondición: Existe un sector con dicho id.
Eficiencia: O(log S)
f) tripulantesN :: Nave -> [Tripulante]
Propósito: Devuelve la lista de tripulantes ordenada por rango, de mayor a menor.
Eficiencia: O(log T)
g) agregarASector :: [Componente] -> SectorId -> Nave -> Nave
Propósito: Asigna una lista de componentes a un sector de la nave.
Eficiencia: O(C + log S), siendo C la cantidad de componentes dados.
h) asignarASector :: Nombre -> SectorId -> Nave -> Nave
Propósito: Asigna un sector a un tripulante.
Nota: No importa si el tripulante ya tiene asignado dicho sector.
Precondición: El tripulante y el sector existen.
Eficiencia: O(log S + log T + T log T)


Sector, siendo C la cantidad de contenedres y T la cantidad de tripulantes:
crearS :: SectorId -> Sector O(1)
sectorId :: Sector -> SectorId O(1)
componentesS :: Sector -> [Componente] O(1)
tripulantesS :: Sector -> Set Nombre O(1)
agregarC :: Componente -> Sector -> Sector O(1)
agregarT :: Nombre -> Sector -> Sector O(log T)



Tripulante, siendo S la cantidad de sectores:
crearT :: Nombre -> Rango -> Tripulante O(1)
asignarS :: SectorId -> Tripulante -> Tripulante
O(log S)
sectoresT :: Tripulante -> Set SectorId O(1)
nombre :: Tripulante -> String O(1)
rango :: Tripulante -> Rango O(1)



Set, siendo N la cantidad de elementos del conjunto:
emptyS :: Set a O(1)
addS :: a -> Set a -> Set a O(log N)
belongsS :: a -> Set a -> Bool O(log N)
unionS :: Set a -> Set a -> Set a O(N log N)
setToList :: Set a -> [a] O(N)
sizeS :: Set a -> Int O(1)


MaxHeap, siendo M la cantidad de elementos en la heap:
emptyH :: MaxHeap a O(1)
isEmptyH :: MaxHeap a -> Bool O(1)
insertH :: a -> MaxHeap a -> MaxHeap a O(log M)
maxH :: MaxHeap a -> a O(1)
deleteMaxH :: MaxHeap a -> MaxHeap a O(log M)



Map, siendo K la cantidad de claves distintas en el map:
emptyM :: Map k v O(1)
assocM :: k -> v -> Map k v -> Map k v O(log K)
lookupM :: k -> Map k v -> Maybe v O(log K)
deleteM :: k -> Map k v -> Map k v O(log K)
domM :: Map k v -> [k] O(K)



-}
