import Text.Show.Functions

data Jugador = UnJugador {
    nombre :: String,
    nombre_padre :: String,
    habilidades :: Habilidad
} deriving (Show, Eq)

data Habilidad = UnaHabilidad{
    fuerzaJugador :: Int, 
    precisionJugador :: Int
} deriving (Show, Eq)

-- Jugadores de ejemplo
bart, todd, rafa :: Jugador
bart = UnJugador "Bart" "Homero" (UnaHabilidad 25 95)
todd = UnJugador "Todd" "Ned" (UnaHabilidad 15 80)
rafa = UnJugador "Rafa" "Gorgory" (UnaHabilidad 10 1)

between n m x = elem x [n .. m]

data Tiro = UnTiro {
  velocidad :: Int,
  precision :: Int,
  altura :: Int
} deriving (Eq, Show)

type Palo = Habilidad -> Tiro

putter :: Palo
putter habilidad = UnTiro 10 (precisionJugador habilidad *2) 0

madera :: Palo
madera habilidad = UnTiro 100 (precisionJugador habilidad `div` 2) 5

hierro :: Int -> (Palo)
hierro n habilidad 
    | n<= 10 && n>= 1 = UnTiro (fuerzaJugador habilidad * n) (precisionJugador habilidad `div` n) ((n-3) `max` 0)
    | otherwise = error "n fuera del rango"

palos :: [Palo]
palos = [putter, madera] ++ map hierro [1..10]

golpe :: Jugador -> Palo -> Tiro
golpe persona palo = palo (habilidades persona)


data Obstaculo = UnObstaculo {
    puedeSuperar :: Tiro -> Bool, 
    efectoSuperar :: Tiro -> Tiro
} deriving (Show)

tunel :: Obstaculo
tunel = UnObstaculo superarTunel efectoTunel

superarTunel :: Tiro -> Bool
superarTunel tiro = ((precision tiro) > 90) && (altura tiro == 0)

efectoTunel :: Tiro -> Tiro
efectoTunel tiro = UnTiro ((velocidad tiro) * 2) 100 0

laguna :: Int -> Obstaculo
laguna largo = UnObstaculo superarLaguna (efectoLaguna largo)

superarLaguna :: Tiro -> Bool 
superarLaguna tiro = (velocidad tiro) > 80 && (between 1 5 (altura tiro))

efectoLaguna ::Int -> Tiro -> Tiro
efectoLaguna largo tiro= UnTiro (velocidad tiro) (precision tiro) (altura tiro `div` largo)

hoyo :: Obstaculo
hoyo = UnObstaculo superarHoyo efectoHoyo

superarHoyo :: Tiro -> Bool
superarHoyo tiro =between 5 20 (velocidad tiro) && altura tiro == 0 &&  precision tiro > 95

efectoHoyo _ = tiroDetenido

tiroDetenido :: Tiro
tiroDetenido = UnTiro 0 0 0 

palosUtiles :: Jugador -> Obstaculo -> [Palo]
palosUtiles jugador obstaculo = filter (leSirveParaObstaculo jugador obstaculo) palos

leSirveParaObstaculo :: Jugador -> Obstaculo -> Palo -> Bool
leSirveParaObstaculo jugador obstaculo palo = puedeSuperar obstaculo (golpe jugador palo)

cantidadObstaculosConsecutivos :: Tiro ->[Obstaculo] -> Int
cantidadObstaculosConsecutivos tiro [] = 0
cantidadObstaculosConsecutivos tiro (obstaculo : obstaculos) 
    | puedeSuperar obstaculo tiro = 1 + cantidadObstaculosConsecutivos (efectoSuperar obstaculo tiro) obstaculos
    | otherwise = 0 

--maximoSegun :: ord b => (a -> b) -> [a] -> a
maximoSegun f = foldl1 (mayorSegun f)

--mayorSegun :: ord x => (t->x)-> (t->t->t)
mayorSegun f a b
  | f a > f b = a
  | otherwise = b


paloMasUtil :: Jugador -> [Obstaculo] -> Palo
paloMasUtil persona obstaculos =maximoSegun (flip cantidadObstaculosConsecutivos obstaculos.golpe persona) palos

type Puntos = Int

jugadorDeTorneo = fst
puntosGandos = snd

pierdenApuesta :: [(Jugador, Puntos)] -> [String]
pierdenApuesta puntosToreno = 