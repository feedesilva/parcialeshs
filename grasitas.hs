import Text.Show.Functions

data Persona = UnaPersona {
    edad :: Int,
    peso :: Float, 
    coefTonificacion :: Int
} deriving (Show, Eq, Ord)

type Ejercicio = Int -> Persona -> Persona

saludable :: Persona -> Bool
saludable persona = not (obeso persona) && coefTonificacion persona > 5

obeso :: Persona -> Bool
obeso = (> 100).peso 

quemarCalorias :: Persona -> Int -> Persona
quemarCalorias persona calorias
    | obeso persona = persona {peso = peso persona - fromIntegral(calorias `div` 150)}
    | not (obeso persona) && edad persona > 30 && calorias > 200 = persona {peso = peso persona -1}
    | otherwise = persona {peso = peso persona - (fromIntegral calorias / (fromIntegral(edad persona) * peso persona))}

cinta :: Int -> Ejercicio
cinta velocidad minutos persona = quemarCalorias persona ((velocidad * minutos) `div` 60)

caminata :: Ejercicio
caminata minutos persona = cinta 5 minutos persona

entrenamientoEnCinta :: Ejercicio
entrenamientoEnCinta minutos persona = cinta (((6*minutos) `div` 60) + (minutos `div` 60)) minutos persona 

pesas :: Int -> Ejercicio 
pesas kilos minutos persona
    | minutos > 10 = persona {coefTonificacion = coefTonificacion persona + kilos `div`10}
    | otherwise = id persona

colina :: Int -> Ejercicio
colina inclinacion minutos persona = quemarCalorias persona (2*minutos*inclinacion)

montaña :: Int -> Ejercicio
montaña inclinacion minutos  = incrementarTonificacion . (colina ((+3)inclinacion) (minutos `div`2))  . (colina inclinacion (minutos `div` 2))

incrementarTonificacion :: Persona -> Persona
incrementarTonificacion persona =persona {coefTonificacion = coefTonificacion persona + 1}

hacerEjercicio ::  Int -> Persona -> (Int -> Persona -> Persona) -> Persona
hacerEjercicio minutos persona ejercicio = ejercicio minutos persona

hacerRutina :: Persona -> (String, Int, [Ejercicio]) -> Persona
hacerRutina persona (nombre, duracion, ejercicios) = foldr1 (hacerEjercicio persona) ejercicios 



-- ejemplos -- 
pancho, andres :: Persona
pancho = UnaPersona 40 120 1
andres = UnaPersona 22 80 6

