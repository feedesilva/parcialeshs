data Persona = UnaPersona {
    habilidades :: [String],
    esBuena :: Bool
} deriving (Show, Eq)

data PowerRanger = UnPowerRanger{
    color :: String,
    habilidadesPower :: [String],
    nivelPelea :: Int
} deriving (Show, Eq)

diego, maxi, fran, coco :: Persona
diego = UnaPersona  ["alto", "grande"] True
maxi = UnaPersona  ["bajo"] False
fran = UnaPersona  ["gorda", "vizca" , "floja"] False
coco = UnaPersona  ["lista", "pete", "rota", "tete", "cuca"] True

negro, verde, azul,rojo :: PowerRanger 
negro = UnPowerRanger "negro" ["fuego"] 2
verde = UnPowerRanger "verde" ["fuego"] 32
azul = UnPowerRanger "azul" ["fuego"] 6
rojo = UnPowerRanger "rojo" ["lista", "pete", "rota", "tete", "cuca", ""] 25


convertirEnPowerRanger :: (String, Persona) -> PowerRanger
convertirEnPowerRanger (color, persona) = UnPowerRanger color (map ("super"++) (habilidades persona)) (length (habilidades persona))

formarEquipoRanger :: [String] -> [Persona] -> [PowerRanger]
formarEquipoRanger colores personas = map (convertirEnPowerRanger) (personasBuenas colores personas)

personasBuenas :: [String] -> [Persona] -> [(String, Persona)]
personasBuenas colores personas = zip colores (filter (esBuena) personas)

findOrElse ::Eq a => (a -> Bool) -> a -> [a] -> a
findOrElse condicion valor lista 
    | length lista > length (filter condicion lista)  = valor
    | (filter condicion lista) /= []= head(filter condicion lista)
    | otherwise = valor

rangerLider :: [PowerRanger] -> String
rangerLider powers
    = findOrElse (/="rojo") "rojo" (map color powers) 

maximumBy ::Eq a=> [a] -> (a->Int) -> Int
maximumBy lista f = maximum (map f lista) 

rangerMásPoderoso :: [PowerRanger] -> Int
rangerMásPoderoso equipo = (maximumBy equipo nivelPelea)

rangerHabilidoso :: PowerRanger -> Bool
rangerHabilidoso ranger = length(habilidadesPower ranger) > 5

alfa5 :: PowerRanger
alfa5 = UnPowerRanger "metalico" ["reparar cosas", concat(repeat "ay"), "rr"] 0

