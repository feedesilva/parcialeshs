import Text.Show.Functions
data Persona = UnaPersona{
    nombre :: String,
    dinero :: Float,
    suerte :: Int,
    factores :: [(String, Int)]
} deriving (Show)


suerteAmuleto :: Persona -> [a]
suerteAmuleto  persona
    | elem "amuleto" (map fst (factores persona)) = map f $ filter esAmuleto (factores persona)
        --(filter (=="amuleto") (map fst (factores persona)))  
    | otherwise = error "no da"
 --   | elem "amuleto" (map (fst factores) persona) = persona {suerte = suerte persona}

f (_,b) = b

 esAmuleto :: (String, Int) -> Bool
 esAmuleto (b,_) = (b == "amuleto")
 -- (=="amuleto") (map fst (factores persona))
-- Ejemplos --
nico = (UnaPersona "Nico" 100.0 30 [("amuleto", 3), ("manos magicas",100)])
maiu = (UnaPersona "Maiu" 100.0 42 [("inteligencia",55), ("paciencia",50)])
