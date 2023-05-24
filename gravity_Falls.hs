data Persona = UnaPersona {
    edad :: Int, 
    items :: [String],
    experiencia :: Int
}deriving (Show, Eq)

data Criatura = UnaCriatura{
    peligrosidad :: Int, 
    condicionDeshacer :: String
}

enfrentar :: Persona ->  Criatura -> Persona
enfrentar persona criatura
    | elem (condicionDeshacer criatura) (items persona) = persona {experiencia = experiencia persona + peligrosidad criatura}
    | otherwise = persona {experiencia = experiencia persona + 1}

experienciaEnfrentarCriaturas :: Persona -> [Criatura] -> Int
--experienciaEnfrentarCriaturas persona criaturas = sum (map (experiencia) (map (enfrentar persona) criaturas))
experienciaEnfrentarCriaturas persona criaturas = foldr1 (+) (map (experiencia)  (map (enfrentar persona)criaturas))

siempredetras :: Criatura
siempredetras = UnaCriatura 0 ""

gnomos :: Int -> Criatura
gnomos n = UnaCriatura (2^n) "soplador de hojas"

fantasmas :: Int -> Criatura
fantasmas poder = UnaCriatura ((*20)poder) "jj"

dipper :: Persona
dipper = UnaPersona 13 ["soplador de hojas"] 0