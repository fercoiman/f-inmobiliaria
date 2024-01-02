module Library where
import PdePreludat

-- sumarHorrocrux mago = mago {cantHorrorcruxes = cantHorrorcruxes mago + 1}
-- bizcocho = UnPostre ["borracho", "fruta", "crema"] 100 20



doble :: Number -> Number
doble numero = numero + numero
-- Modelo inicial
data Jugador = UnJugador {
  nombre :: String,
  padre :: String,
  habilidad :: Habilidad
} deriving (Eq, Show)
data Habilidad = Habilidad {
  fuerzaJugador :: Number,
  precisionJugador :: Number
} deriving (Eq, Show)

-- Jugadores de ejemplo
bart = UnJugador "Bart" "Homero" (Habilidad 25 60)
todd = UnJugador "Todd" "Ned" (Habilidad 15 80)
rafa = UnJugador "Rafa" "Gorgory" (Habilidad 10 1)


data Tiro = UnTiro{
  velocidad :: Number,
  precision :: Number,
  altura :: Number
} deriving (Eq, Show)

type Puntos = Number

-- Funciones útiles
between :: (Eq a, Enum a) => a -> a -> a -> Bool
between n m x = elem x [n .. m]

maximoSegun :: Ord a1 => (a2 -> a1) -> [a2] -> a2
maximoSegun f = foldl1 (mayorSegun f)

mayorSegun :: Ord a => (p -> a) -> p -> p -> p
mayorSegun f a b
  | f a > f b = a
  | otherwise = b

type Palo = Habilidad -> Tiro

putter::Habilidad-> Tiro
putter habilidad =  UnTiro{
   velocidad = 10, 
   precision =  precisionJugador habilidad * 2,
   altura = 0
   }

madera::Palo
madera habilidad =  UnTiro{
   velocidad = 100, 
   precision =  precisionJugador habilidad / 2,
   altura = 5  
   }

hierro::Number-> (Habilidad -> Tiro)
hierro n habilidad =  UnTiro{
   velocidad = 100, 
   precision =  precisionJugador habilidad / 2,
   altura = 5  
   }

palos :: [Palo]
palos = [putter,madera] ++ map hierro [1..9] ++ [hierro 10]

golpe :: Jugador -> Palo -> Tiro
golpe jugador palo =  palo (habilidad jugador)




