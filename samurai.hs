data Elemento = UnElemento { 
    tipo :: String,
	ataque :: (Personaje-> Personaje),
    defensa :: (Personaje-> Personaje) 
    }

data Personaje = UnPersonaje { 
    nombre :: String,
	salud :: Float,
    elementos :: [Elemento],
    anioPresente :: Int
    }

mandarAlAnio :: Int -> Personaje -> Personaje
mandarAlAnio año personaje = personaje {anioPresente = año}

meditar :: Personaje -> Personaje
meditar personaje = personaje {salud = salud personaje * 1.5}

causarDanio :: Float -> Personaje -> Personaje
causarDanio n personaje = personaje {salud = max (salud personaje - n) 0}

esMalvado :: Personaje -> Bool
esMalvado  = any (esDeTipo "malvado"). elementos 

esDeTipo untipo elemento = untipo == tipo elemento

danioQueProduce :: Personaje -> Elemento -> Float
danioQueProduce personaje elemento = salud personaje - salud(ataque elemento personaje)

enemigosMortales :: Personaje -> [Personaje] -> [Personaje]
enemigosMortales personaje enemigos = filter (esEnemigoMortal personaje) enemigos
esEnemigoMortal personaje enemigo =
  (any (tieneAtaqueMortal personaje) . elementos) enemigo

tieneAtaqueMortal personaje elemento =
  danioQueProduce personaje elemento == salud personaje


jack :: Personaje
jack = UnPersonaje {
  nombre = "Jack",
  salud = 300,
  elementos = [concentracion 3, katanaMagica],
  anioPresente = 200
}
katanaMagica = UnElemento "Magia" (causarDanio 1000) noHacerNada
