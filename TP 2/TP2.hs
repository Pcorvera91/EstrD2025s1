
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

ponkemonsDe :: Entrenador -> [Pokemon]
pokemonsDe (ConsEntrenador _ pkms) = pkms

cantPokemonDe :: TipoDePokemon -> Entrenador -> Int
cantPokemonDe t e = cantPokemon (entrenadorConPokemonsDeTipo e t)

entrenadorConPokemonsDeTipo :: Entrenador -> TipoDePokemon -> Entrenador
entrenadorConPokemonsDeTipo e t = ConsEntrenador n (pokemonsDeTipo e t)

pokemonsDeTipo :: Entrenador -> TipoDePokemon -> [Pokemons]
pokemonsDeTipo


