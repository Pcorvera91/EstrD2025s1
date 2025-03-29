import Distribution.Backpack.PreModuleShape (PreModuleShape(preModShapeProvides))
import Graphics.Win32 (pS_ALTERNATE)
import System.Win32.DebugApi (rsi)

sumatoria :: [Int] -> Int
sumatoria [] = 0
sumatoria (x : xs) = x + sumatoria xs

longitud :: [a] -> Int
longitud [] = 0
longitud (x : xs) = 1 + longitud xs

sucesores :: [Int] -> [Int]
sucesores [] = []
sucesores (n : ns) = sucesor n : sucesores ns

sucesor :: Int -> Int
sucesor n = n + 1

conjuncion :: [Bool] -> Bool
conjuncion [] = True
conjuncion (b : bs) = b && conjuncion bs

disyuncion :: [Bool] -> Bool
disyuncion [] = False
disyuncion (b : bs) = b || disyuncion bs

aplanar :: [[a]] -> [a]
aplanar [] = []
aplanar (x : xs) = x ++ aplanar xs

pertenece :: (Eq a) => a -> [a] -> Bool
pertenece x [] = False
pertenece x (y : ys) = (x == y) || pertenece x ys

apariciones :: (Eq a) => a -> [a] -> Int
apariciones x [] = 0
apariciones x (y : ys) = es1SiSino0 (x == y) + apariciones x ys

es1SiSino0 :: Bool -> Int
es1SiSino0 condicion =
  if (condicion)
    then 1
    else 0

losMenoresA :: Int -> [Int] -> [Int]
losMenoresA n [] = []
losMenoresA n (m : ms) = elMenor m n : losMenoresA n ms

elMenor :: Int -> Int -> Int
elMenor n m = if n > m then n else m

lasDeLongitudMayorA :: Int -> [[a]] -> [[a]]
lasDeLongitudMayorA n [] = []
lasDeLongitudMayorA n (x : xs) =
  if (longitud x > n)
    then x : lasDeLongitudMayorA n xs
    else lasDeLongitudMayorA n xs

agregarAlFinal :: [a] -> a -> [a]
agregarAlFinal [] x = [x]
agregarAlFinal (y:ys) x = y : agregarAlFinal ys x

reversa :: [a] -> [a]
reversa [] = []
reversa (x:xs) = agregarAlFinal (reversa xs) x

zipMaximos :: [Int] -> [Int] -> [Int]
zipMaximos [] _ = []
zipMaximos _ [] = []
zipMaximos (n:ns) (m:ms) = maxDelPar (n,m) : zipMaximos ns ms

maxDelPar :: (Int, Int) -> Int
maxDelPar (n, m) = if n > m then n else m


elMinimo :: Ord a => [a] -> a
-- Precondición: la lista no está vacía
elMinimo [] = error "La lista está vacía, no se puede hallar el mínimo"
elMinimo (x:xs) = menor x (elMinimo xs)

menor :: Ord a => a -> a -> a
menor x y = if x < y then x else y

factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial (n-1)

cuentaRegresiva :: Int -> [Int]
cuentaRegresiva 0 = []
cuentaRegresiva n = n : cuentaRegresiva (n-1)

repetir :: Int -> a -> [a]
repetir 0 x = []
repetir n x = x : repetir (n-1) x

losPrimeros :: Int -> [a] -> [a]
losPrimeros 0 _ = []
losPrimeros _ [] = []
losPrimeros n (x:xs) = x : losPrimeros (n-1) xs

sinLosPrimeros :: Int -> [a] -> [a]
sinLosPrimeros _ [] = []
sinLosPrimeros n (x:xs) = if n<=0
                          then x : sinLosPrimeros (n-1) xs
                          else sinLosPrimeros (n-1) xs


data Persona = P String Int String
  deriving (Show)

mayoresA :: Int -> [Persona] -> [Persona]
mayoresA n [] = []
mayoresA n (p:ps) = if edad p > n
                    then p : mayoresA n ps
                    else mayoresA n ps

edad :: Persona -> Int
edad (P n e d) = e

promedioEdad :: [Persona] -> Int
--Precond: La lista al menos posee una persona
promedioEdad ps = promedio (edades ps)

edades :: [Persona] -> [Int]
edades [] = []
edades (p:ps) = edad p : edades ps

promedio :: [ Int ] -> Int
-- PRECOND: la lista no es vacía
promedio ns = div (sumatoria ns) (longitud ns)

elMasViejo :: [Persona] -> Persona
--PRECOND: La lista al menos tiene una persona
elMasViejo [] = error "Tiene que haber al menos una persona"
elMasViejo (p:ps) = laQueEsMayor p (elMasViejo ps)

laQueEsMayor :: Persona -> Persona -> Persona
laQueEsMayor p1 p2 = if edad p1 > edad p2 then p1 else p2

data TipoDePokemon = Agua | Fuego | Planta
data Pokemon = ConsPokemon TipoDePokemon Int
data Entrenador = ConsEntrenador String [Pokemon]

cantPokemon :: Entrenador -> Int
cantPokemon e = longitud (pokemonsDe e)

pokemonsDe :: Entrenador -> [Pokemon]
pokemonsDe (ConsEntrenador n pkms) = pkms 

cantPokemonDe :: TipoDePokemon -> Entrenador -> Int
cantPokemonDe t e = cantPokemon (entrenadorConPokemonsDeTipo e t)

entrenadorConPokemonsDeTipo :: Entrenador -> TipoDePokemon -> Entrenador
entrenadorConPokemonsDeTipo e t = ConsEntrenador (nombreEntrenador e) (pokemonsDeTipo (pokemonsDe e) t)

nombreEntrenador :: Entrenador -> String
nombreEntrenador (ConsEntrenador n pkms) = n

pokemonsDeTipo :: [Pokemon] -> TipoDePokemon -> [Pokemon]
pokemonsDeTipo [] _ = []
pokemonsDeTipo (pkm:pkms) t = if (esDeTipo t pkm)
                              then pkm : pokemonsDeTipo pkms t
                              else pokemonsDeTipo pkms t

esDeTipo :: TipoDePokemon -> Pokemon -> Bool
esDeTipo Agua (ConsPokemon Agua _) = True
esDeTipo Fuego (ConsPokemon Fuego _) = True
esDeTipo Planta (ConsPokemon Planta _) = True
esDeTipo _ _ = False

cuantosDeTipo_De_LeGananATodosLosDe_ :: TipoDePokemon -> Entrenador -> Entrenador -> Int
-- cuantosDeTipo_De_LeGananATodosLosDe_ t e1 e2 = st t (pokemones e1) (pokemones e2)
cuantosDeTipo_De_LeGananATodosLosDe_ t e1 e2 = cuantosPokemonsLeGananATodosA (pokemonsDeTipo  (pokemonsDe e1) t) (pokemonsDe e2)

cuantosPokemonsLeGananATodosA :: [Pokemon] -> [Pokemon] -> Int
cuantosPokemonsLeGananATodosA [] pk2s = 0
cuantosPokemonsLeGananATodosA pk1s [] = longitud pk1s
cuantosPokemonsLeGananATodosA  (pk1:pk1s) pk2s = es1SiSino0 (leGanaATodos pk1 pk2s ) + cuantosPokemonsLeGananATodosA  pk1s pk2s


leGanaATodos :: Pokemon -> [Pokemon] -> Bool
leGanaATodos _ [] = True
leGanaATodos pk (pkm:pkms) = superaA pk pkm && leGanaATodos pk pkms 

superaA :: Pokemon -> Pokemon -> Bool
superaA p1 p2 = tipoSuperaA (tipo p1) (tipo p2)

tipoSuperaA :: TipoDePokemon -> TipoDePokemon -> Bool
tipoSuperaA Agua Fuego = True
tipoSuperaA Fuego Planta = True
tipoSuperaA Planta Agua = True
tipoSuperaA _ _ = False

tipo :: Pokemon -> TipoDePokemon
tipo (ConsPokemon t _) = t


esMaestroPokemon :: Entrenador -> Bool
esMaestroPokemon e = hayPokemonsDeDiferenteTipo (pokemonsDe e)

hayPokemonsDeDiferenteTipo :: [Pokemon] -> Bool
hayPokemonsDeDiferenteTipo pkms = hayAlMenosUnoDe_En_ Agua pkms && 
                                  hayAlMenosUnoDe_En_ Fuego pkms &&
                                  hayAlMenosUnoDe_En_ Planta pkms

hayAlMenosUnoDe_En_ :: TipoDePokemon -> [Pokemon] -> Bool
hayAlMenosUnoDe_En_ t [] = False
hayAlMenosUnoDe_En_ t (pkm:pkms) = esDeTipo t pkm || hayAlMenosUnoDe_En_ t pkms


data Seniority = Junior | SemiSenior | Senior
data Proyecto = ConsProyecto String
data Rol = Developer Seniority Proyecto | Management Seniority Proyecto
data Empresa = ConsEmpresa [Rol]

proyectos :: Empresa -> [Proyecto]
proyectos e = proyectosDeLaEmpresa e

proyectosDeLaEmpresa :: Empresa -> [Proyecto]
proyectosDeLaEmpresa (ConsEmpresa rs) = proyectosDeDiferentesRoles rs

proyectosDeDiferentesRoles :: [Rol] -> [Proyecto]
proyectosDeDiferentesRoles [] = []
proyectosDeDiferentesRoles (r:rs) = agregarProyectoSiNoEsta (proyecto r)  (proyectosDeDiferentesRoles rs)


agregarProyectoSiNoEsta :: Proyecto -> [Proyecto] -> [Proyecto]
agregarProyectoSiNoEsta p [] = []
agregarProyectoSiNoEsta p (pr:prs) = if not ( sonElMismoProyecto p pr)
                                     then pr : agregarProyectoSiNoEsta p prs
                                     else agregarProyectoSiNoEsta p prs
 
sonElMismoProyecto :: Proyecto -> Proyecto -> Bool
sonElMismoProyecto p1 p2 = nombre p1 == nombre p2

nombre :: Proyecto -> String
nombre (ConsProyecto n) = n

proyecto :: Rol -> Proyecto
proyecto (Developer _ p) = p
proyecto (Management _ p) = p





losDevSenior :: Empresa -> [Proyecto] -> Int
losDevSenior (ConsEmpresa rs) prs = rolesQueEstanEnAlgunProyecto (rolesQueSonDevSenior rs)  prs 

rolesQueEstanEnAlgunProyecto :: [Rol] -> [Proyecto] -> Int
rolesQueEstanEnAlgunProyecto [] ps = 0
rolesQueEstanEnAlgunProyecto (r:rs) [] = 0
rolesQueEstanEnAlgunProyecto (r:rs) ps = es1SiSino0 (elRolEstaEnAlgunProyecto r ps ) + rolesQueEstanEnAlgunProyecto rs ps

elRolEstaEnAlgunProyecto :: Rol -> [Proyecto] -> Bool
elRolEstaEnAlgunProyecto r [] = False
elRolEstaEnAlgunProyecto r (p:ps) = sonElMismoProyecto (proyecto r) p || elRolEstaEnAlgunProyecto r ps 

rolesQueSonDevSenior :: [Rol] -> [Rol]
rolesQueSonDevSenior [] = []
rolesQueSonDevSenior (r:rs) = if (esDevSenior r)
                              then r : rolesQueSonDevSenior rs
                              else rolesQueSonDevSenior rs

esDevSenior :: Rol -> Bool
esDevSenior (Developer Senior p) = True
esDevSenior _ = False

cantQueTrabajanEn :: [Proyecto] -> Empresa -> Int
cantQueTrabajanEn ps (ConsEmpresa rs) = rolesQueEstanEnAlgunProyecto rs ps 

asignadosPorProyecto :: Empresa -> [(Proyecto, Int)]
asignadosPorProyecto e = proyectosConSuCantidadDeTrabajadores (proyectos e) (roles e)

proyectosConSuCantidadDeTrabajadores :: [Proyecto] -> [Rol] -> [(Proyecto,Int)]
proyectosConSuCantidadDeTrabajadores []  rs = []
proyectosConSuCantidadDeTrabajadores ps  [] = []
proyectosConSuCantidadDeTrabajadores (p:ps) rs = (p,cantDeTrabajadoresQueTrabajanEn rs p) : proyectosConSuCantidadDeTrabajadores ps rs


cantDeTrabajadoresQueTrabajanEn :: [Rol] -> Proyecto -> Int
cantDeTrabajadoresQueTrabajanEn [] p = 0
cantDeTrabajadoresQueTrabajanEn (r:rs) p = es1SiSino0 (sonElMismoProyecto (proyecto r) p ) + cantDeTrabajadoresQueTrabajanEn rs p 

roles :: Empresa -> [Rol]
roles (ConsEmpresa rs) = rs




