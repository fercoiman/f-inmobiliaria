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

between :: Ord a => a -> a -> a -> Bool
between cotaInferior cotaSuperior valor =
 valor <= cotaSuperior && valor >= cotaInferior

deptosDeEjemplo = [
 Depto 3 80 7500 "Palermo",
 Depto 1 45 3500 "Villa Urquiza",
 Depto 2 50 5000 "Palermo",
 Depto 1 45 5500 "Recoleta"]


-- Punto 1
mayor :: Ord a => (t -> a) -> t -> t -> Bool
mayor funcion x y = funcion x > funcion y


menor :: Ord a => (t -> a) -> t -> t -> Bool
menor funcion x y = funcion x < funcion y

type Lista = [String]
lista = ["uno","quince","ocho"]

-- Punto 2

ubicadoEn :: [Barrio] -> (Depto -> Bool)
ubicadoEn barriosDeInteres departamento = elem (barrio departamento) barriosDeInteres

ubicadoEn' :: [Barrio] -> Depto -> Bool
ubicadoEn' barriosDeInteres departamento = flip elem barriosDeInteres. barrio $ departamento


ubicadoEn'' :: [Barrio] -> Depto -> Bool
ubicadoEn'' barriosDeInteres  = flip elem barriosDeInteres. barrio


cumpleRango :: Ord a1 => (Depto -> a1) -> a1 -> a1 -> Depto -> Bool
cumpleRango criterio valorInf valorSup departamento =  between valorInf valorSup. criterio $ departamento

cumpleRango' :: Ord a1 => (a2 -> a1) -> a1 -> a1 -> a2 -> Bool
cumpleRango' criterio valorInf valorSup  =  between valorInf valorSup. criterio

