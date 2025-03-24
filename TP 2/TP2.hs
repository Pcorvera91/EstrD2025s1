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




