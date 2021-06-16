type CorreccionDeVelocidad = Chocobo -> Int

data Tramo = Tramo {
  distancia :: Int,
  correccionDeVelocidad :: CorreccionDeVelocidad
}

data Chocobo = Chocobo {
  fuerza :: Int, 
  peso :: Int,
  velocidad :: Int 
} deriving (Show, Eq)

data Jinete = Jinete {
  nombre :: String,
  chocobo :: Chocobo
} deriving (Show, Eq)

type Pista = [Tramo]

amarillo = Chocobo 5 3 3 
negro = Chocobo 4 4 4
blanco = Chocobo 2 3 6 
rojo = Chocobo 3 3 4 

f1 chocobo = velocidad chocobo * 2 
f2 chocobo = velocidad chocobo + fuerza chocobo 
--f3 chocobo = velocidad chocobo / peso chocobo
f3 chocobo = velocidad chocobo `div` peso chocobo

bosqueTenebroso = [
  Tramo 100 f1,
  Tramo 50 f2,
  Tramo 120 f2, 
  Tramo 200 f1,
  Tramo 80 f3 ] 

pantanoDelDestino = [
  Tramo 40 f2,
  Tramo 90 (\c -> fuerza c + peso c + velocidad c),
  Tramo 120 fuerza,
  Tramo 20 fuerza ]


--Finalmente, estos chocobos están dirigidos por los 4 jinetes: 
apocalipsis = [
  Jinete "Leo" amarillo,
  Jinete "Gise" blanco,
  Jinete "Mati" negro,
  Jinete "Alf" rojo ] 

quickSort :: (a -> a -> Bool) -> [a] -> [a]
quickSort _ [] = [] 
quickSort criterio (x:xs) =
  (quickSort criterio . filter (not . criterio x)) xs  ++ 
  [x] ++  
  (quickSort criterio . filter (criterio x)) xs 


--1. Definir dos funciones mayorSegun y menorSegun que, dados una función y dos valores, nos dice si el resultado de  evaluar la función para el primer valor es mayor / menor que el resultado de evaluar la función para el segundo. 
--Ejemplo: mayorSegun length bosqueTenebroso pantanoDelDestino _>_>_>_ True (tiene 5 tramos el bosque y 4 tramos el pantano) 

mayorSegun :: Ord b => (a -> b) -> a -> a -> Bool
mayorSegun criterio a b = criterio a > criterio b 

menorSegun :: Ord b => (a -> b) -> a -> a -> Bool
menorSegun criterio a b = criterio a < criterio b 

---------------------
-- 2. 
-- a. Saber el tiempo que tarda un chocobo en recorrer un tramo. El mismo está dado por la distancia del tramo  dividido por la velocidad corregida para el chocobo. 
-- Ejemplo: tiempo amarillo (head bosqueTenebroso) _>_>_>_ 16 

tiempo :: Chocobo -> Tramo -> Int
tiempo chocobo tramo = distancia tramo `div` correccionDeVelocidad tramo chocobo

---------------------
-- b. Determinar el tiempo total de un chocobo en una carrera. 
-- Ejemplo: tiempoTotal bosqueTenebroso amarillo _>_>_>_ 150 

tiempoTotal :: Pista -> Chocobo -> Int
tiempoTotal pista = sum.flip map pista.tiempo

---------------------
-- 3. Obtener el podio de una carrera, representado por una lista ordenada de los 3 primeros puestos de la misma, en  base a una lista de jinetes y una pista. El puesto está dado por el tiempo total, de menor a mayor y se espera obtener una lista de jinetes. 
--Ejemplo: podio bosqueTenebroso apocalipsis _>_>_>_ [("Gise",(2,3,6)),("Mati",(4,4,4)),("Alf",(3,3,4))] (ver también ejemplo del punto 6) 

podio :: Pista -> [Jinete] -> [Jinete]
podio pista = take 3.quickSort (menorSegun (tiempoTotal pista.chocobo))

---------------------
--4. a. Realizar una función que dado un tramo y una lista de jinetes, retorna el nombre de aquel que lo recorrió en  el menor tiempo. 
--Ejemplo: elMejorDelTramo (head bosqueTenebroso) apocalipsis _>_>_>_ "Gise" (Gise tarda 8, mientras que Leo tarda 16 y Mati y Alf tardan 12) 

elMejorDelTramo :: Tramo -> [Jinete] -> String
elMejorDelTramo tramo = nombre.head.podio [tramo]

---------------------
--b. Dada una pista y una lista de jinetes, saber el nombre del jinete que ganó más tramos (que no quiere decir que haya ganado la carrera). 
--Ejemplo: elMasWinner pantanoDelDestino apocalipsis _>_>_>_ "Leo" (gana 2 tramos, el resto gana 1 o ninguno) 

tramosGanados :: Pista -> [Jinete] -> Jinete -> Int
tramosGanados pista jinetes supuestoGanador = (length.filter (== nombre supuestoGanador).map (`elMejorDelTramo` jinetes)) pista

elMasWinner :: Pista -> [Jinete] -> String
elMasWinner pista jinetes = (nombre.head.quickSort (mayorSegun (tramosGanados pista jinetes))) jinetes

---------------------
--5. Saber los nombres de los jinetes que pueden hacer un tramo dado en un tiempo indicado máximo.. 
--Ejemplo: quienesPueden (head bosqueTenebroso) 12 apocalipsis _>_>_>_ ["Gise","Mati","Alf"] (ver 4.a) 

quienesPueden :: Tramo -> Int -> [Jinete] -> [String]
quienesPueden tramo tiempoMaximo = map nombre.filter ((tiempoMaximo>=).(`tiempo` tramo).chocobo)

---------------------
--6. Obtener las estadísticas de una carrera, dada la pista y la lista de jinetes. Estas estadísticas deben estar  representadas por una lista de tuplas, cada tupla siendo de la forma: (nombre, tramosGanados, tiempoTotal) 
--Ejemplo: estadisticas bosqueTenebroso apocalipsis _>_>_>_ [("Leo",0,150),("Gise",3,85),("Mati",2,138),("Alf",0,141)] 

estadisticas :: Pista -> [Jinete] -> [(String,Int,Int)]
estadisticas pista jinetes = map (\unJinete -> (nombre unJinete, (tramosGanados pista jinetes) unJinete, (tiempoTotal pista.chocobo) unJinete )) jinetes

---------------------
--7. Saber si una carrera fue pareja. Esto es así si cada chocobo tuvo un tiempo total de hasta 10% menor que el que llegó a continuación. 
--Ejemplo: fuePareja bosqueTenebroso apocalipsis _>_>_>_ False (entre Gise y Mati, 1ª y 2º respectivamente, hay más de 10% de diferencia) 

correrPista :: Pista -> Jinete -> Int
correrPista pista = tiempoTotal pista.chocobo

diferenciaPorcentual :: Int -> Int -> Int 
diferenciaPorcentual a = (subtract 100).(*100).(`div` a)

fueParejaEntreDos :: Int -> Int -> Bool
fueParejaEntreDos a = (>10).diferenciaPorcentual a

fuePareja :: Pista -> [Jinete] -> Bool
fuePareja _ [] = False
fuePareja pista (c0:c1:[]) = fueParejaEntreDos (correrPista pista c0) (correrPista pista c1)
fuePareja pista (c0:c1:xs) = fueParejaEntreDos (correrPista pista c0) (correrPista pista c1) && fuePareja pista xs

---------------------
--8. Definir un chocobo plateado que tenga las mejores características de los otros (mayor fuerza, menor peso, mayor  velocidad), teniendo en cuenta que no sea necesario cambiar su definición si se altera un valor de los anteriores. 
--Ejemplo: plateado _>_>_>_ (5,3,6) 

chocobos = [ amarillo, negro, blanco, rojo ]

plateado = Chocobo ((maximum.map fuerza) chocobos) ((minimum.map peso) chocobos) ((maximum.map velocidad) chocobos)

---------------------
--9. Definir el tipo de funcionHeavy: 
--funcionHeavy :: Ord b => [(b,b)] -> (b,b) -> ((b,b) -> [a]) -> [c]
funcionHeavy x y z 
  | (fst . head) x < snd y = map z x 
  | otherwise = filter (fst y ==) (map z x)
