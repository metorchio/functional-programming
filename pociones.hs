data Persona = Persona {
  nombrePersona :: String,
  suerte :: Int,
  inteligencia :: Int,
  fuerza :: Int
} deriving (Show, Eq)

data Pocion = Pocion {
  nombrePocion :: String,
  ingredientes :: [Ingrediente]
}

type Efecto = Persona -> Persona

data Ingrediente = Ingrediente {
  nombreIngrediente :: String,
  efectos :: [Efecto]
}

nombresDeIngredientesProhibidos = [
 "sangre de unicornio",
 "veneno de basilisco",
 "patas de cabra",
 "efedrina"]

maximoSegun :: Ord b => (a -> b) -> [a] -> a
maximoSegun _ [ x ] = x
maximoSegun  f ( x : y : xs)
  | f x > f y = maximoSegun f (x:xs)
  | otherwise = maximoSegun f (y:xs)


--------------------
--Casos de prueba
ejemplosDeMagos = [
  Persona "El Mago mala onda" 8 3 5,
  Persona "Magu" 8 8 2,
  Persona "Harry Postre" 5 3 3 ]

bajaFuerza :: Persona -> Int -> Persona
bajaFuerza persona cant = persona { fuerza = fuerza persona - cant }
subeFuerza :: Persona -> Int -> Persona
subeFuerza persona cant = persona { fuerza = fuerza persona + cant }
bajaInteligencia :: Persona -> Int -> Persona
bajaInteligencia persona cant = persona { inteligencia = inteligencia persona - cant }
bajaSuerte :: Persona -> Int -> Persona
bajaSuerte persona cant = persona { suerte = suerte persona - cant }

azuquita = Ingrediente "azucar" [(\p -> p{inteligencia = inteligencia p + 10} )]
efedrina = Ingrediente "efedrina" [(\p -> subeFuerza p 10 )]
ejemplosDePociones = [
  Pocion "antidoto de poc 1" [ Ingrediente "ing 4" [(\p -> subeFuerza p 3 )] ],
  Pocion "poc 1" [ Ingrediente "ing 1" [(\p -> bajaFuerza p 3 )] ],
  Pocion "poc 2" [ azuquita ],
  Pocion "poc 3" [ Ingrediente "ing 2" [(\p -> subeFuerza p 3 )], Ingrediente "ing 3" [(\p -> bajaInteligencia p 3 )] ],
  Pocion "poc hardcore" [ Ingrediente "ing 2" [(\p -> subeFuerza p 3 ), (\p -> bajaInteligencia p 3 ), (\p -> bajaSuerte p 3), (\p -> bajaFuerza p 5)] ],
  Pocion "poc prohibida" [ efedrina ] ]

--------------------
-- 1. Dada una persona definir las siguientes funciones para cuantificar sus niveles de suerte, inteligencia y fuerza *sin repetir código*:

-- Funcion planteada en el video.
niveles :: Persona -> [Int]
niveles persona = [fuerza persona, inteligencia persona, suerte persona]

-- 1.a *sumaDeNiveles* que suma todos sus niveles.
sumaDeNiveles :: Persona -> Int
sumaDeNiveles = sum.niveles 

--------------------
-- 1.b *diferenciaDeNiveles* es la diferencia entre el nivel más alto y más bajo.
diferenciaDeNiveles :: Persona -> Int
diferenciaDeNiveles persona = maximoNivel persona - minimoNivel persona

maximoNivel :: Persona -> Int
maximoNivel = maximum.niveles
minimoNivel :: Persona -> Int
minimoNivel = minimum.niveles
--------------------
-- 1.c *nivelesMayoresA n*, que indica la cantidad de niveles de la persona que están por encima del valor dado.
nivelesMayoresA :: Int -> Persona -> Int
nivelesMayoresA rango = length.filter (>rango).niveles

--------------------
-- 2. Definir la función efectosDePocion que dada una poción devuelve una lista con los efectos de todos sus ingredientes.

efectosDePocion :: Pocion -> [Efecto]
efectosDePocion = concatMap efectos.ingredientes

--------------------
-- 3. Dada una lista de pociones, consultar:
-- 3.a Los *nombres de las pociones hardcore*, que son las que tienen al menos 4 efectos.

nombresPocionesHardcore :: [Pocion] -> [String]
nombresPocionesHardcore = map nombrePocion.filter ((>=4).length.efectosDePocion)

--------------------
-- 3.b La *cantidad de pociones prohibidas*, que son aquellas que tienen algún ingrediente cuyo nombre figura en la lista de ingredientes prohibidos.

esIngredienteProhibido :: Ingrediente -> Bool
esIngredienteProhibido =  flip elem nombresDeIngredientesProhibidos.nombreIngrediente 

cantidadPocionesProhibidas :: [Pocion] -> Int
cantidadPocionesProhibidas = length.filter (any esIngredienteProhibido).map ingredientes

--------------------
-- 3.c Si *son todas dulces*, lo cual ocurre cuando todas las pociones de la lista tienen algún ingrediente llamado “azúcar”.

esDulce :: Ingrediente -> Bool
esDulce = ("azucar"==).nombreIngrediente

sonTodasDulces :: [Pocion] -> Bool
sonTodasDulces = all (any esDulce).map ingredientes

--Resolucion planteada en el video
sonTodasDulces' :: [Pocion] -> Bool
sonTodasDulces' = all (any esDulce . ingredientes)

--------------------
-- 4. Definir la función tomarPocion que recibe una poción y una persona, y devuelve como quedaría la persona después de tomar la poción. Cuando una persona toma una poción, se aplican todos los efectos de esta última, en orden.

tomarPocion :: Pocion -> Persona -> Persona
tomarPocion pocion personaInicial = (foldl (\persona efecto -> efecto persona) personaInicial.efectosDePocion) pocion

--------------------
-- 5. Definir la función esAntidotoDe que recibe dos pociones y una persona, y dice si tomar la segunda poción revierte los cambios que se producen en la persona al tomar la primera.

esAntidotoDe :: Pocion -> Pocion -> Persona -> Bool
esAntidotoDe pocion antidoto personaInicial = ( (==personaInicial).tomarPocion antidoto.tomarPocion pocion) personaInicial

--------------------
-- 6. Definir la función personaMasAfectada que recibe una poción, una función cuantificadora (es decir, una función que dada una persona retorna un número) y una lista de personas, y devuelve a la persona de la lista que hace máxima el valor del cuantificador. Mostrar un ejemplo de uso utilizando los cuantificadores definidos en el punto 1.

personaMasAfectada :: Pocion -> (Persona -> Int) -> [Persona] -> Persona
personaMasAfectada pocion criterio = maximoSegun (criterio. tomarPocion pocion)

ejemploPersonaMasAfectada = personaMasAfectada (ejemplosDePociones!!1) sumaDeNiveles ejemplosDeMagos
ejemploPersonaMasAfectada' = personaMasAfectada (ejemplosDePociones!!1) (nivelesMayoresA 2) ejemplosDeMagos