data Aspecto = UnAspecto {
  tipoDeAspecto :: String,
  grado :: Float
} deriving (Show, Eq)
type Situacion = [Aspecto]
-- tensión, incertidumbre y peligro --

mejorAspecto mejor peor = grado mejor < grado peor
mismoAspecto aspecto1 aspecto2 = tipoDeAspecto aspecto1 == tipoDeAspecto aspecto2
buscarAspecto aspectoBuscado = head.filter (mismoAspecto aspectoBuscado)
buscarAspectoDeTipo tipo = buscarAspecto (UnAspecto tipo 0)
reemplazarAspecto aspectoBuscado situacion =
    aspectoBuscado : (filter (not.mismoAspecto aspectoBuscado) situacion)

modificarAspecto :: (Float -> Float) -> Aspecto -> Aspecto
modificarAspecto f aspecto = aspecto {grado = f (grado aspecto)}

mejorSituacion :: Situacion -> Situacion -> Bool
mejorSituacion s1 s2 = map mejorAspecto 