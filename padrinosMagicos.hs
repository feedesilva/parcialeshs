import Text.Show.Functions

data Chico = UnChico{
    nombre :: String,
    edad :: Int,
    habilidades :: [String],
    deseos :: [Deseo]
} deriving (Show)


type Deseo = Chico -> Chico

aprenderHabilidades :: [String] -> Deseo
aprenderHabilidades habilidadesNuevas chico = chico {habilidades = habilidades chico ++ habilidadesNuevas} 

serGrosoEnNeedForSpeed :: Deseo
serGrosoEnNeedForSpeed chico = chico {habilidades = habilidades chico ++ repeat "need for speed"}
{-
needForSpeed ::Int -> [String] 
needForSpeed n = repeat ("jugar need for speed " ++ aumentar n)

aumentar = (+1)
-}
serMayor :: Deseo
serMayor chico = chico {edad = 18}

wanda :: Deseo
wanda chico = (madurar . cumplirPrimerDeseo chico) chico

cumplirPrimerDeseo :: Chico -> Deseo
cumplirPrimerDeseo chico = head (deseos chico)
madurar :: Deseo
madurar chico = chico {edad = edad chico +1}

cosmo :: Deseo
cosmo = desMadurar

desMadurar :: Deseo
desMadurar chico = chico{edad = edad chico `div` 2 } 
--{-
muffinMagico :: Chico -> Chico
muffinMagico chico =  foldr concederDeseo chico (obtenerDeseos chico)  
-- -})
concederDeseo :: Deseo -> Chico  -> Chico
concederDeseo deseo chico = deseo chico

obtenerDeseos :: Chico -> [Deseo]
obtenerDeseos chico =  deseos chico

tieneHabilidad :: String -> Chico -> Bool
tieneHabilidad habilidad chico = elem habilidad (habilidades chico)

trixie = UnChico "trixie" 9 ["lll"] [(aprenderHabilidades ["cocer"]), serMayor]