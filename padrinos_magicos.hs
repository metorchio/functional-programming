--A. Concediendo deseos

--1. Desarrollar los siguiente deseos y declarar el data Chico
type Habilidad = String

type Deseo = Chico -> Chico

data Chico = Chico {
  nombreChico :: String,
  edad :: Int,
  habilidades :: [Habilidad],
  deseos :: [Deseo]
}
----------------------
-- \--a. aprenderHabilidades habilidades unChico : agrega una lista de habilidades nuevas a las que ya tiene el chico.
aprenderHabilidades :: [Habilidad] -> Deseo
aprenderHabilidades habilidadesNuevas chico = chico{habilidades = ((habilidadesNuevas ++). habilidades) chico }

----------------------
-- \--b. serGrosoEnNeedForSpeed unChico: dado un chico, le agrega las habilidades de jugar a todas las versiones pasadas y futuras del Need For Speed, que son: “jugar need for speed 1”, “jugar need for speed 2”, etc.
serGrosoEnNeedForSpeed :: Deseo
serGrosoEnNeedForSpeed = aprenderHabilidades ["jugar need for speed 1", "jugar need for speed 2"] 

----------------------
-- \--c. serMayor unChico: Hace que el chico tenga 18 años.
serMayor :: Deseo
serMayor chico = chico{ edad = 18 } 

----------------------
--2. Los padrinos son seres mágicos capaces de cumplir los deseos de sus ahijados. Desarrollar los siguientes padrinos:
-- \--a. wanda: dado un chico, wanda le cumple el primer deseo y lo hace madurar(crecer un año de edad).

madurar :: Chico -> Chico
madurar chico = chico{ edad = edad chico + 1 } 

desmadurar :: Chico -> Chico
desmadurar chico = chico{ edad = edad chico `div` 2 } 

wanda :: Chico -> Chico
wanda chico = (madurar.(head.deseos) chico) chico

----------------------
-- \--b. cosmo: dado un chico, lo hace “des”madurar, quedando con la mitad de años de edad. Como es olvidadizo, no le concede ningún deseo.
cosmo :: Chico -> Chico
cosmo chico = desmadurar chico 

----------------------
-- \--c. muffinMagico: dado un chico le concede todos sus deseos.
concederTodosLosDeseos :: Chico -> Chico
concederTodosLosDeseos chico = (foldr ($) chico.deseos) chico

muffinMagico :: Chico -> Chico
muffinMagico = concederTodosLosDeseos

--Nota importante: no debe haber lógica repetida entre wanda, cosmo y serMayor

----------------------
--B. En busqueda de pareja
--1. Se acerca el baile de fin de año y se quiere saber cuáles van a ser las parejas. Para esto las chicas tienen condiciones para elegir al chico con el que van a salir, algunas de ellas son:

-- \--a. tieneHabilidad unaHabilidad unChico: Dado un chico y una habilidad, dice si la posee.

tieneHabilidad :: Habilidad -> Chico -> Bool
tieneHabilidad habilidad = elem habilidad.habilidades

-- \--b. esSuperMaduro: Dado un chico dice si es mayor de edad (es decir, tiene más de 18 años) y además sabe manejar.

esSuperMaduro :: Chico -> Bool
esSuperMaduro chico = ((>18).edad) chico && (tieneHabilidad "manejar") chico

----------------------
--2. Las chicas tienen un nombre, y una condición para elegir al chico con el que van ir al baile. Ejemplos:
data Chica = Chica {
  nombreChica :: String,
  condicion :: (Chico -> Bool) 
}

-- para Trixie la única condición es que el chico no sea Timmy, ya que nunca saldría con él
--trixie = Chica “Trixie Tang” noEsTimmy
--vicky = Chica “Vicky” (tieneHabilidad “ser un supermodelo noruego”)
noEsTimmy :: Chico -> Bool
noEsTimmy = (/="Timmy").nombreChico

trixie :: Chica
trixie = Chica "Trixie Tang" noEsTimmy

vicky :: Chica
vicky = Chica "Vicky" (tieneHabilidad "ser un supermodelo noruego")

--Se pide definir el data Chica y desarrollar las siguientes funciones:
-- \--a. quienConquistaA unaChica losPretendientes: Dada una chica y una lista de pretendientes, devuelve al que se queda con la chica, es decir, el primero que cumpla con la condición que ella quiere. Si no hay ninguno que la cumpla, devuelve el último pretendiente (una chica nunca se queda sola). (Sólo en este punto se puede usar recursividad) 



quienConquistaA :: Chica -> [Chico] -> Chico
quienConquistaA chica pretendientes = (head.filter (condicion chica)) pretendientes

-- \--b. Dar un ejemplo de consulta para una nueva chica, cuya condición para elegir a un chico es que este sepa cocinar.

nuevaChica :: Chica
nuevaChica = Chica "Nueva" (tieneHabilidad "cocinar")

----------------------
--C. Da Rules
-- Como no todo está permitido en el mundo mágico, Jorgen Von Strángulo está encargado de controlar que se no se viole lo establecido en “da Rules”:

--1. infractoresDeDaRules : Dada una lista de chicos, devuelve la lista de los nombres de aquellos que tienen deseos prohibidos. Un deseo está prohibido si, al aplicarlo, entre las cinco primeras habilidades, hay alguna prohibida. En tanto, son habilidades prohibidas enamorar, matar y dominar el mundo.

habilidadesProhibidas = ["matar", "enamorar", "dominar al mundo" ]

esHabilidadProhibida :: Habilidad -> Bool
esHabilidadProhibida = (`elem` habilidadesProhibidas)

infractoresDeDaRules :: [Chico] -> [String]
infractoresDeDaRules = 
  map nombreChico.filter ( any esHabilidadProhibida.take 5.habilidades ).map concederTodosLosDeseos