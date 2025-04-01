sucesor :: Int -> Int
sucesor n = n + 1

sumar :: Int -> Int -> Int
sumar n m = n + m

divisionYResto :: Int -> Int -> (Int, Int)
divisionYResto n m = (div n m, mod n m)

maxDelPar :: (Int, Int) -> Int
maxDelPar (n, m) = if n > m then n else m

data Dir = Norte | Sur | Este | Oeste
  deriving (Show)

opuesto :: Dir -> Dir
opuesto Norte = Sur
opuesto Este = Oeste
opuesto Oeste = Este
opuesto Sur = Norte

iguales :: Dir -> Dir -> Bool
iguales Norte Norte = True
iguales Sur Sur = True
iguales Oeste Oeste = True
iguales Este Este = True
iguales _ _ = False

siguiente :: Dir -> Dir
siguiente Norte = Este
siguiente Este = Sur
siguiente Sur = Oeste
siguiente Oeste = error "No existe siguiente de oeste"

data DiaDeSemana = Lunes | Martes | Miercoles | Jueves | Viernes | Sabado | Domingo

primeroYUltimoDia :: (DiaDeSemana, DiaDeSemana)
primeroYUltimoDia = (Lunes, Domingo)

empiezaConM :: DiaDeSemana -> Bool
empiezaConM Martes = True
empiezaConM Miercoles = True
empiezaConM _ = False

vieneDespues :: DiaDeSemana -> DiaDeSemana -> Bool
vieneDespues Lunes Martes = True
vieneDespues Martes  Miercoles = True
vieneDespues Miercoles Jueves = True
vieneDespues Jueves Viernes = True
vieneDespues Viernes Sabado = True
vieneDespues Sabado Domingo = True
vieneDespues Domingo Lunes = True
vieneDespues _ _ = False

estaEnElMedio :: DiaDeSemana -> Bool
estaEnElMedio Lunes = False
estaEnElMedio Domingo = False
estaEnElMedio _ = True

negar :: Bool -> Bool
negar True = False
negar False = True

implica :: Bool -> Bool -> Bool
implica True False = False
implica _ _ = True

yTambien :: Bool -> Bool -> Bool
yTambien True True = True
yTambien _ _ = False

oBien :: Bool -> Bool -> Bool
oBien False False = False
oBien _ _ = True

data Persona = P String Int String
  deriving (Show)

nombre :: Persona -> String
nombre (P n e d) = n

edad :: Persona -> Int
edad (P n e d) = e

crecer :: Persona -> Persona
crecer (P n e d) = P n (e + 1) d

cambioDeNombre :: String -> Persona -> Persona
cambioDeNombre nom (P n e d) = P nom e d

esMayorQueLaOtra :: Persona -> Persona -> Bool
esMayorQueLaOtra p1 p2 = edad p1 > edad p2

laQueEsMayor :: Persona -> Persona -> Persona
laQueEsMayor p1 p2 = if edad p1 > edad p2 then p1 else p2

data TipoDePokemon = Agua | Fuego | Planta
  deriving (Show)

data Pokemon = Pk TipoDePokemon Int
  deriving (Show)

data Entrenador = E String Pokemon Pokemon
  deriving (Show)

superaA :: Pokemon -> Pokemon -> Bool
superaA p1 p2 = tipoSuperaA (tipo p1) (tipo p2)

tipoSuperaA :: TipoDePokemon -> TipoDePokemon -> Bool
tipoSuperaA Agua Fuego = True
tipoSuperaA Fuego Planta = True
tipoSuperaA Planta Agua = True
tipoSuperaA _ _ = False

tipo :: Pokemon -> TipoDePokemon
tipo (Pk t _) = t

cantidadDePokemonDe :: TipoDePokemon -> Entrenador -> Int
cantidadDePokemonDe t e = es1SiSino0 (esDeTipo t (pokemon1 e)) + es1SiSino0 (esDeTipo t (pokemon2 e))

esDeTipo :: TipoDePokemon -> Pokemon -> Bool
esDeTipo Agua (Pk Agua _) = True
esDeTipo Fuego (Pk Fuego _) = True
esDeTipo Planta (Pk Planta _) = True
esDeTipo _ _ = False

pokemon1 :: Entrenador -> Pokemon
pokemon1 (E _ p1 _) = p1

pokemon2 :: Entrenador -> Pokemon
pokemon2 (E _ _ p2) = p2

es1SiSino0 :: Bool -> Int
es1SiSino0 condicion =
  if (condicion)
    then 1
    else 0

juntarPokemon :: (Entrenador, Entrenador) -> [Pokemon]
juntarPokemon (e1, e2) = pokemonsDe e1 ++ pokemonsDe e2

pokemonsDe :: Entrenador -> [Pokemon]
pokemonsDe e = [pokemon1 e, pokemon2 e]

loMismo :: a -> a
loMismo x = x

siempreSiete :: a -> Int
siempreSiete x = 7

swap :: (a, b) -> (b, a)
swap (x, y) = (y, x)

-- Porque pueden tomar diferentes tipos de valores

estaVacia :: [a] -> Bool
estaVacia [] = True
estaVacia _  = False


elPrimero :: [a] -> a
elPrimero (x : xs) = x

sinElPrimero :: [a] -> [a]
sinElPrimero (x : xs) = xs

splitHead :: [a] -> (a, [a])
splitHead (x : xs) = (elPrimero (x : xs), sinElPrimero (x : xs))

-- maxDelPar (divisionYResto (sumar (sucesor 8) 1) (sucesor 0))
-- maxDelPar (divisionYResto (sumar (sucesor 4) (sucesor 4)) (sucesor 0))
-- maxDelPar (divisionYResto (sumar (sucesor (sucesor 3)) (sucesor (sucesor 3))) (sucesor 0))
-- maxDelPar (divisionYResto (sumar (sucesor 2) (sumar 3 4)) (sucesor 0))