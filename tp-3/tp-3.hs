data Color = Azul | Rojo
data Celda = Bolita Color Celda | CeldaVacia

nroBolitas :: Color -> Celda -> Int

nroBolitas _ CeldaVacia = 0
nroBolitas colorBuscado (Bolita colorCelda cel) =
  es1SiSino0 (elColorBuscadoEsIgualAlDeLaCelda colorBuscado colorCelda) + nroBolitas colorBuscado cel

es1SiSino0 :: Bool -> Int
es1SiSino0 True  = 1
es1SiSino0 False = 0

elColorBuscadoEsIgualAlDeLaCelda :: Color -> Color -> Bool
elColorBuscadoEsIgualAlDeLaCelda cb cc = cb == cc

poner :: Color -> Celda -> Celda
poner colorAPoner CeldaVacia = Bolita colorAPoner CeldaVacia
poner colorAPoner (Bolita cc c) = Bolita colorAPoner (Bolita cc c)

sacar :: Color -> Celda -> Celda
sacar colorASacar CeldaVacia = 
sacar colorASacar (Bolita col c) =



