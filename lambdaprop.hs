type Barrio = String
type Mail = String
type Requisito = Depto -> Bool
type Busqueda = [Requisito]

data Depto = Depto {
 ambientes :: Int,
 superficie :: Int,
 precio :: Int,
 barrio :: Barrio
} deriving (Show, Eq)

data Persona = Persona {
   mail :: Mail,
   busquedas :: [Busqueda]
}

ordenarSegun :: (a -> a -> Bool) -> [a] -> [a]
ordenarSegun _ [] = []
ordenarSegun criterio (x:xs) =
 (ordenarSegun criterio . filter (not . criterio x)) xs ++
 [x] ++
 (ordenarSegun criterio . filter (criterio x)) xs

between :: Ord a => a -> a -> a -> Bool
between cotaInferior cotaSuperior valor =
 valor <= cotaSuperior && valor >= cotaInferior

deptosDeEjemplo = [
 Depto 3 80 7500 "Palermo",
 Depto 1 45 3500 "Villa Urquiza",
 Depto 2 50 5000 "Palermo",
 Depto 1 45 5500 "Recoleta"]

r1 = (>42).superficie
r2 = (<5500).precio
b1 = [r1, r2]
personasDeEjemplo = [
   Persona "fake@email.com" [b1],
   Persona "otro@email.com" [ [ (<5500).precio, (==1).ambientes ], [r1] ],
   Persona "un@tercero.com" [ [(==7500).precio, (==1).ambientes] ] ]

mayor :: Ord b => ( a -> b ) -> (a -> a -> Bool)
mayor func p1 p2 = func p1 > func p2

menor :: Ord b => ( a -> b ) -> (a -> a -> Bool)
menor func p1 p2 = func p1 < func p2

ejemploDeOrdenarSegun = ordenarSegun (mayor length) ["un string", "otro string", "otro mas"]

ubicadoEn' :: Depto -> [Barrio] -> Bool
ubicadoEn' dpto = any (barrio dpto ==)
ubicadoEn :: [Barrio] -> Requisito
ubicadoEn barrios = (`elem` barrios).barrio
--ubicadoEn (deptosDeEjemplo!!1) ["Villa Urquiza"]

cumpleRango :: Ord b => (Depto -> b) -> b -> b -> Requisito
cumpleRango func mayor menor = (between mayor menor).func

cumpleBusqueda :: Depto -> Busqueda -> Bool
cumpleBusqueda dpto = all (\requisito -> requisito dpto)
--uso: cumpleBusqueda ((>49).precio) deptosDeEjemplo

buscar :: Busqueda -> (Depto -> Depto -> Bool) -> [Depto] -> [Depto] 
buscar criterioBusqueda ordenamiento = ordenarSegun ordenamiento.filter (flip cumpleBusqueda criterioBusqueda)

ejemploUsoBuscar = buscar [ ubicadoEn ["Palermo", "Recoleta"], 
                            cumpleRango ambientes 1 2, 
                            cumpleRango precio 0 6000 ] 
                          (mayor superficie) deptosDeEjemplo

mailsDePersonasInteresadas :: Depto -> [Persona] -> [Mail]
mailsDePersonasInteresadas dpto = map mail.filter (any (cumpleBusqueda dpto).busquedas)

ejemploMailsDePersonasInteresadas = mailsDePersonasInteresadas (deptosDeEjemplo!!2) personasDeEjemplo