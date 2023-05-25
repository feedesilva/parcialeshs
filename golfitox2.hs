-- Modelo inicial
data Jugador = UnJugador {
  nombre :: String,
  padre :: String,
  habilidad :: Habilidad
} deriving (Eq, Show)

data Habilidad = Habilidad {
  fuerzaJugador :: Int,
  precisionJugador :: Int
} deriving (Eq, Show)

-- Jugadores de ejemplo
bart = UnJugador "Bart" "Homero" (Habilidad 25 60)
todd = UnJugador "Todd" "Ned" (Habilidad 15 80)
rafa = UnJugador "Rafa" "Gorgory" (Habilidad 10 1)

data Tiro = UnTiro {
  velocidad :: Int,
  precision :: Int,
  altura :: Int
} deriving (Eq, Show)

type Puntos = Int

-- Funciones útiles
between n m x = elem x [n .. m]

maximoSegun f = foldl1 (mayorSegun f)
mayorSegun f a b
  | f a > f b = a
  | otherwise = b

-- Empiezo a trabajar -- 

type Palo = Habilidad -> Tiro

putter :: Palo 
putter habilidad = UnTiro 10 ((*2)(precisionJugador habilidad)) 0
madera :: Palo 
madera habilidad = UnTiro 100  (precisionJugador habilidad `div` 2) 5
hierro :: Int -> Palo 
hierro n habilidad =  UnTiro ((*n) (fuerzaJugador habilidad)) (precisionJugador habilidad `div` n) (max (n-3) 0)