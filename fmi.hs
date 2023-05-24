
data Pais = UnPais{
    ingresoxcapita :: Float, -- promedio de lo que cada habitante necesita para subsistir -- 
    poblacionPublico :: Integer,
    poblacionPrivado :: Integer,
    recursosNat :: [String],
    deuda :: Float --Millones U$D -- 
} 
    deriving (Show, Eq)

type Receta = Pais -> Pais


prestarle :: Float -> Receta
prestarle n pais = pais {deuda = deuda pais + n*1.5 } 

reducirPublico :: Integer -> Receta
reducirPublico cantidad pais 
    | cantidad > 100 = pais {ingresoxcapita = ingresoxcapita pais * 0.8} {poblacionPublico = poblacionPublico pais - cantidad}
    | otherwise = pais {ingresoxcapita = ingresoxcapita pais * 0.85} {poblacionPublico = poblacionPublico pais - cantidad}

explotarRecursos :: String -> Receta
explotarRecursos recurso pais = pais {deuda = deuda pais - 2} {recursosNat = filter(/=recurso) (recursosNat pais)}

blindaje :: Receta
blindaje pais = ((reducirPublico 500) . (prestarle(pbi pais/2))) pais

pbi :: Pais -> Float
pbi pais = ingresoxcapita pais * (fromInteger(poblacionPrivado pais + poblacionPublico pais))

-- 3 a -- 
miReceta :: Receta
miReceta pais = (explotarRecursos "mineria" . prestarle 200) pais

-- 3 b -- 

aplicarReceta :: Pais -> [Receta] -> Pais
aplicarReceta pais recetas = foldr ($) pais recetas

-- 4 a -- 
puedenZafar :: [Pais] -> [Pais]
puedenZafar = filter (elem "petroleo". recursosNat)

totalDeuda :: [Pais] -> Float
--totalDeuda  = foldr ((+) . deuda) 0
totalDeuda = sum.map deuda

-- 5 --
recursosNaturalesInfinitos :: [String]
recursosNaturalesInfinitos = "Energia" : recursosNaturalesInfinitos


-- Ejemplos -- 
namibia :: Pais
namibia = UnPais 4140 400000 650000 ["mineria", "ecoturismo"] 50
venezuela :: Pais
venezuela = UnPais 2010 50000 2500 ["petroleo", "madera"] 5222
argentina :: Pais
argentina = UnPais 251  10585   2651    ["vacas", "mar"] 25
qatar :: Pais
qatar = UnPais 1551 252536  96546   ["petroleo"] 1

