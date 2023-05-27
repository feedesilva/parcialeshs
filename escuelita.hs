import Text.Show.Functions

data Guantalete = UnGuantalete{
    material :: String, 
    gemas :: [Gema]
} deriving (Show)

type Gema = Personaje -> Personaje

data Personaje = UnPersonaje{
    nombre :: String,
    edad :: Int,
    energia :: Int,
    habilidades :: [String],
    planeta :: String
} deriving (Show)

type Universo = [Personaje]

chasquearUniverso :: Guantalete -> Universo -> Universo
chasquearUniverso guantalete universo 
    | puedeUsarse guantalete = reducirMitad universo
    | otherwise = universo

puedeUsarse :: Guantalete -> Bool
puedeUsarse guantalete = (length (gemas guantalete) == 6 && (material guantalete) == "uru")

reducirMitad :: Universo -> Universo
reducirMitad universo = take (div (length universo)  2) universo

aptoPendex :: Universo -> Bool
aptoPendex  = any (<=45).(map edad) 

{-
energiaTotal :: Universo -> Int
energiaTotal = foldr (+) (energia habilidosos)
-}

habilidosos :: Universo -> Int
habilidosos universo = foldr1 (+) (map energia (filter ((>1).length.habilidades) universo))

-- Segunda Parte -- 

quitarEnergia :: Int -> Gema
quitarEnergia valor personaje = personaje {energia = energia personaje - valor}

laMente :: Int -> Gema
laMente = quitarEnergia

elAlma :: String -> Gema
elAlma habilidad  = quitarEnergia 10 . quitarHabilidad habilidad

quitarHabilidad :: String -> Gema
quitarHabilidad habilidad personaje = personaje {habilidades = filter (/=habilidad) (habilidades personaje)}

elEspacio :: String -> Gema
elEspacio planeta  = quitarEnergia 20 . transportarRival planeta

transportarRival :: String -> Gema
transportarRival nuevoPlaneta personaje = personaje {planeta = nuevoPlaneta}

elPoder :: Gema
elPoder personaje
    | ((<=2).length.habilidades) personaje = personaje {energia = 0, habilidades = []}
    | otherwise = personaje {energia = 0}

elTiempo :: Gema
elTiempo  = disminuirEdad . quitarEnergia 50 

disminuirEdad :: Gema
disminuirEdad personaje = personaje {edad = max (edad personaje `div` 2) 18}

gemaLoca :: Gema -> Gema
gemaLoca gema = gema.gema

-- Punto 4 --
ejemplo :: Guantalete
ejemplo = UnGuantalete "Goma" [elTiempo, elAlma "usar Mjolnir", gemaLoca (elAlma "programacion en Haskell")]

utilizar :: [Gema] -> Personaje -> Personaje
utilizar gemas enemigo = foldr ($) enemigo gemas

gemaMasPoderosa :: Personaje -> Guantalete -> Gema
gemaMasPoderosa personaje guantelte = compararPoder personaje $ gemas guantelte

compararPoder :: Personaje -> [Gema] -> Gema
compararPoder _ [gema] = gema
compararPoder personaje (gema1:gema2:gemas)
    | (energia.gema1) personaje > (energia.gema2) personaje = compararPoder personaje (gema1:gemas)
    | otherwise = compararPoder personaje (gema2:gemas)


infinitasGemas :: Gema -> [Gema]
infinitasGemas gema = gema:(infinitasGemas gema)

guanteleteDeLocos :: Guantalete
guanteleteDeLocos = UnGuantalete "vesconite" (infinitasGemas elTiempo)

usoLasTresPrimerasGemas :: Guantalete -> Personaje -> Personaje
usoLasTresPrimerasGemas guantelete = (utilizar. take 3. gemas) guantelete

punisher:: Personaje 
punisher = UnPersonaje "The Punisher" 38 350 ["Disparar con de todo","golpear"] "Tierra" 
{-
gemaMasPoderosa punisher guanteleteDeLocos
usoLasTresPrimerasGemas guanteleteDeLocos punisher
-}
-- Ejemplos -- 

javier, facu, juanma, jose :: Personaje
javier = UnPersonaje "javi" 35 25 ["chasquear"] "Tierra"
facu = UnPersonaje "facu" 56 85 ["roncar","callar"] "Jupiter"
juanma = UnPersonaje "juanma" 20 255 ["murmurar"] "Tierra"
jose = UnPersonaje "jose" 69 5 ["chasquear", "jugar", "pegar"] "Marte"
