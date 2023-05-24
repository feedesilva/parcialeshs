import Text.Show.Functions

data Turista = UnTurista {
    nivelCansancio :: Int,
    nivelEstres :: Int, 
    viajaSolo :: Bool,
    idiomas :: [String]
} deriving (Show, Eq)

type Excursion = Turista -> Turista 

ana, beto, cathi :: Turista
ana = UnTurista 0 21 False ["espagnol"]
beto = UnTurista 15 15 True ["aleman"]
cathi = UnTurista 15 15 True ["aleman", "catalan"]

irALaPlaya :: Excursion
irALaPlaya turista 
    | viajaSolo turista  = UnTurista (nivelCansancio turista - 5) (nivelEstres turista) (viajaSolo turista) (idiomas turista)
    | otherwise = UnTurista (nivelCansancio turista) (nivelEstres turista - 1) (viajaSolo turista) (idiomas turista) 

apreciarPaisaje :: String -> Excursion
apreciarPaisaje paisaje turista = UnTurista (nivelCansancio turista) (nivelEstres turista - (length paisaje)) (viajaSolo turista) (idiomas turista)

hablarIdioma :: String -> Excursion
hablarIdioma idioma turista = UnTurista (nivelCansancio turista) (nivelEstres turista) False (idioma : idiomas turista)

caminar :: Int -> Excursion
caminar minutos turista = UnTurista (nivelCansancio turista + (minutos `div` 4)) (nivelEstres turista - (minutos `div` 4)) (viajaSolo turista) (idiomas turista)

paseoEnBarco :: String -> Excursion
paseoEnBarco marea turista
    | marea == "fuerte" = turista {nivelEstres = nivelEstres turista + 6} {nivelCansancio = nivelCansancio turista + 10}
    | marea == "moderada" = id turista
    | marea == "tranquila" = caminar 10 turista
--    | marea == "tranquila" = (caminar 10).(apreciarPaisaje "mar").(hablarIdioma "aleman")

hacerExcursion :: Excursion -> Turista -> Turista
hacerExcursion excursion  = cambiarStress (-10) . excursion

cambiarStress :: Int -> Turista -> Turista
cambiarStress porcentaje turista= turista{nivelEstres = nivelEstres turista- div (porcentaje*nivelEstres turista) 100}

--fc dada --

deltaSegun :: (a -> Int) -> a -> a -> Int
deltaSegun f algo1 algo2 = f algo1 - f algo2

deltaExcursionSegun :: (Turista -> Int) -> Turista -> Excursion -> Int
deltaExcursionSegun indice turista excursion = deltaSegun indice (hacerExcursion excursion turista) turista

esEducativa :: Turista ->Excursion -> Bool
esEducativa  turista 
    = (>0).(deltaExcursionSegun (length.idiomas) turista)

excursionesDesestresantes :: Turista -> [Excursion] -> [Excursion]
excursionesDesestresantes turista  = filter (esDesestresante turista) 

esDesestresante :: Turista -> Excursion -> Bool
esDesestresante turista  = (<= -3).(deltaExcursionSegun nivelEstres turista)

type Tour = [Excursion]

completo :: Tour
completo = [caminar 20, apreciarPaisaje "cascada", caminar 40, irALaPlaya, hablarIdioma "melmacquiano"]

ladoB :: Excursion -> Tour
ladoB excursion = [paseoEnBarco "tranquila", excursion, caminar 120]

islaVecina :: String -> Tour
islaVecina marea
    | marea == "fuerte" = [paseoEnBarco "fuerte",apreciarPaisaje "lago", paseoEnBarco "fuerte"]
    | otherwise = [paseoEnBarco marea, irALaPlaya, paseoEnBarco marea]

hacerTour :: Turista -> Tour -> Turista
hacerTour turista tour = foldl (flip hacerExcursion) (aumentarStres turista tour) tour 

aumentarStres :: Turista -> Tour -> Turista
aumentarStres turista tour = turista {nivelEstres = nivelEstres turista + length tour} 

-- 3B -- 