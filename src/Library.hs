module Library where
import PdePreludat

doble :: Number -> Number
doble numero = numero + numero

type Barrio = String
type Mail = String
type Requisito = Depto -> Bool
type Busqueda = [Requisito]

data Depto = Depto {
 ambientes :: Number,
 superficie :: Number,
 precio :: Number,
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

between cotaInferior cotaSuperior valor =
 valor <= cotaSuperior && valor >= cotaInferior

deptosDeEjemplo = [
 Depto 3 80 7500 "Palermo",
 Depto 1 45 3500 "Villa Urquiza",
 Depto 2 50 5000 "Palermo",
 Depto 1 45 5500 "Recoleta"]


-- Punto 1


mayor f x y = f x > f y


menor f x y = f x < f y

ejemploDeOrdenarSegun :: [Depto]
ejemploDeOrdenarSegun = ordenarSegun (menor ambientes) deptosDeEjemplo


-- Punto 2

ubicadoEn :: [Barrio] -> (Depto -> Bool)
ubicadoEn barrios depto = barrio depto `elem` barrios

ubicadoEn' :: [Barrio] -> Depto -> Bool
ubicadoEn' barrios = (`elem` barrios).barrio



