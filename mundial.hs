data Jugador = UnJugador {
    nombre :: String, 
    edad :: Int,
    promedioGol :: Float,
    habilidad :: Float,
    cansancio :: Float
} deriving (Show, Eq, Ord)

data Equipo = UnEquipo{
    equipo :: String,
    grupo :: Char,
    jugadores :: [Jugador]
} deriving (Show, Eq, Ord)

{-
a partir de una lista y un criterio de ordenamiento, nos devuelve la versión equivalente a esa lista pero con los elementos ordenados por el criterio dado.  

-}

quickSort _ [] = [] 
quickSort criterio (x:xs) = (quickSort criterio . filter (not . criterio x)) xs ++ [x] ++ (quickSort criterio . filter (criterio x)) xs 

figuras :: Equipo -> [Jugador]
figuras equipo = filter esFigura (jugadores equipo)

esFigura :: Jugador -> Bool
esFigura jugador = (habilidad jugador > 75) && (promedioGol jugador > 0)

tieneFarandulero :: Equipo -> Bool
tieneFarandulero equipo  = any esFarandulero (jugadores equipo) 

esFarandulero :: Jugador-> Bool
esFarandulero jugador  = elem (nombre jugador) jugadoresFaranduleros

jugadoresFaranduleros = ["Maxi Lopez", "Icardi", "Aguero", "Caniggia", "Demichelis"]
{-
album :: [Equipo] -> Char -> [String]
album equipos c = (filter nombre .filter jugadoresDificiles . filter (esDeGrupo c)) equipos

jugadoresDificiles :: [Equipo] -> [Bool]
jugadoresDificiles equipos = (filter (esDificil) (map jugadores equipos)) 
-}

esDificil :: Jugador -> Bool
esDificil jugador = (esFigura jugador) && (esJoven jugador) && not(esFarandulero jugador)

esDeGrupo :: Char -> Equipo -> Bool
esDeGrupo c equipo = c == grupo equipo

esJoven :: Jugador -> Bool
esJoven jugador = edad jugador < 27

jugarPartido :: Equipo -> Equipo
jugarPartido equipo 
    | 
-- Ejemplos -- 

martin, juan, maxi, jonathan, lean, brian, garcia, messi, aguero :: Jugador
martin = UnJugador "Martin" 26 0.0 50 35.0
juan = UnJugador "Juancho" 30 0.2 80 40.0
maxi = UnJugador "Maxi Lopez" 27 0.4 68 30.0

jonathan = UnJugador "Chueco" 20 1.5 80 99.0
lean = UnJugador "Hacha" 23 0.01 50 35.0
brian = UnJugador "Panadero" 21 5 80 15.0

garcia = UnJugador "Sargento" 30 1 80 13.0
messi = UnJugador "Pulga" 26 10 99 43.0
aguero = UnJugador "Aguero" 24 5 90 5.0

equipo1, losDeSiempre, restoDelMundo :: Equipo
equipo1 = UnEquipo "Lo Que Vale Es El Intento" 'F' [martin, juan, maxi]
losDeSiempre = UnEquipo "Los De Siempre" 'F' [jonathan, lean, brian]
restoDelMundo = UnEquipo "Resto del Mundo" 'A' [garcia, messi, aguero]
