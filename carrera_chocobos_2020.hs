type Pista = [Tramo]

data Tramo = Tramo {
  longitud :: Int,
  terreno :: Terreno
}

data Chocobo = Chocobo {
  tiempo :: Int, 
  velocidad :: Int,
  color :: String
} deriving (Show, Eq)

--------------------
--1. Modelar una función para ​bajar en cierta cantidad la velocidad a la que un Chocobo corre, y otra para ​hacer que corra una distancia dada​, considerando que recorrer una distancia, le lleva al Chocobo un tiempo equivalente a su velocidad dividida dicha distancia.

bajaVelocidad :: Int -> Chocobo -> Chocobo
bajaVelocidad cant chocobo = chocobo{ velocidad = velocidad chocobo - cant }

correrDistancia :: Int -> Chocobo -> Chocobo
correrDistancia dist chocobo = chocobo{ tiempo = tiempo chocobo + (velocidad chocobo `div` dist) }

--------------------
--2. Definir el tipo ​Terreno​, de forma tal que resulte adecuado para representar los diversos terrenos que los tramos de las pistas atraviesan y…

type Terreno = Chocobo -> Chocobo

--------------------
-- a. Modelar los siguientes terrenos:
-- asfalto​: Este terreno es ideal para correr ya que no impacta de ninguna forma a los Chocobos.

asfalto :: Terreno
asfalto = bajaVelocidad 0

--------------------
-- bosque​: Los múltiples obstáculos y peligros de este terreno obligan a los Chocobos a reducir su velocidad una cuarta parte.

bosque :: Terreno
bosque chocobo = bajaVelocidad ((div 4.velocidad) chocobo) chocobo

--------------------
--agua​: Los tramos sumergidos en agua pueden impactar a las aves de diversas maneras. Si la profundidad del agua es menos de 50 cm, los pájaros son capaces de atravesarla perdiendo dos puntos de velocidad por cada cm de profundidad. Por otro lado, si el agua es más profunda que eso, los Chocobos no se animan a cruzarla y, en cambio, pierden 5 minutos bordeando por el costado. Cabe destacar que estas penalidades no afectan a los Chocobos Azules, que son naturalmente buenos nadadores y cruzan los tramos de agua como si nada.

perderTiempo :: Int -> Chocobo -> Chocobo
perderTiempo tiempoPerdido chocobo = chocobo{ tiempo = tiempo chocobo + tiempoPerdido } 

esChocoboAzul :: Chocobo -> Bool
esChocoboAzul = ("azul"==).color

agua :: Int -> Terreno
agua _ chocobo | esChocoboAzul chocobo = chocobo
agua profundidad chocobo | profundidad < 50 = (bajaVelocidad (profundidad*2)) chocobo
agua _ chocobo | otherwise = perderTiempo 5 chocobo

--------------------
--pantano​: Los pantanos son lugares horribles que mezclan todos los peores aspectos de los bosques y las zonas inundadas. Cuando un Chocobo cruza un pantano lo impacta igual que cruzar un bosque y 20 cm de agua (en ese orden). Además de eso, pierde un punto extra de velocidad, sólo por el cagazo que le da el lugar.

--Considero que primero pierde un punto de velocidad, al ver el terreno, antes de empezar a recorrerlo. 
pantano :: Terreno
pantano = agua 20.bosque.bajaVelocidad 1

--------------------
-- b. Modelar una función ​correrTramo que, dado un Tramo y un Chocobo haga que el ave corra la distancia del tramo luego de ser afectada por el terreno.

correrTramo :: Tramo -> Chocobo -> Chocobo
correrTramo tramo = correrDistancia (longitud tramo).terreno tramo
-- (chocobo { velocidad = velocidad chocobo - (terreno tramo) chocobo})

--------------------
-- c. Dar un ejemplo de pista que contenga tramos con todos los terrenos descritos anteriormente ​y uno más, inventado por vos​, con un comentario que describa qué cambio hace.

ejemploDePista = [
  Tramo 5 (agua 5),
  Tramo 5 pantano,
  Tramo 5 bosque,
  Tramo 5 asfalto,
  Tramo 5 (\chocobo -> bajaVelocidad 3 chocobo) ]

--------------------
--3. Definir funciones que, dada una lista de Chocobos: 
--a. Indique cuál es el mayor tiempo de los Chocobos que todavía están corriendo (es decir, aquellos cuya velocidad es > 0).

mayorTiempo :: [Chocobo] -> Int
mayorTiempo = maximum.map tiempo.filter ((>0).velocidad)

--------------------
--b. Dada una lista de colores, indique si todos los Chocobos que llevan más de treinta minutos corriendo son de un color de la lista.

sonDeColor :: [String] -> [Chocobo] -> Bool
sonDeColor colores = all (`elem` colores) . map color . filter ((>30).tiempo)

--------------------
-- c. Dado un tramo y un Chocobo campeón, indique cuántos de los Chocobos de la lista pueden correr dicho tramo más rápido que el campeón.

masRapidoQueCampeon :: Tramo -> Chocobo -> [Chocobo] -> Int
masRapidoQueCampeon tramo campeon = length.filter (< tiempo campeon).map (tiempo.correrTramo tramo)

--------------------
--4. Utilizando la siguiente función ​ordenar​:

ordenar :: Ord b => (a -> b) -> [a] -> [a]
ordenar _ [] = []
ordenar f (x:xs) = (ordenar f . filter ((f x >=).f)) xs ++
  [x] ++
  (ordenar f . filter ((f x <).f)) xs

--Modelar la función ​correrCarrera :: [Chocobo] -> Pista -> [Chocobo]​, que reciba una lista de participantes y una pista y retorne el podio resultante de la carrera, es decir, los tres Chocobos que recorrieron todos los tramos de la pista (en orden) en el menor tiempo, ordenados de menor a mayor tiempo.

correrPista :: Chocobo -> Pista -> Chocobo
--correrPista chocoboInicial = foldl (\chocobo tramo -> correrTramo tramo chocobo) chocoboInicial
correrPista chocoboInicial = foldl (flip correrTramo) chocoboInicial

correrPista' :: Pista -> Chocobo -> Chocobo
correrPista' pista chocoboInicial = foldl (flip correrTramo) chocoboInicial pista


correrCarrera :: [Chocobo] -> Pista -> [Chocobo]
correrCarrera participantes pista = (take 3.ordenar (\c -> tiempo c).map (`correrPista` pista)) participantes

correrCarrera' :: [Chocobo] -> Pista -> [Chocobo]
correrCarrera' participantes pista = (take 3.ordenar (\c -> tiempo c).map (correrPista' pista)) participantes