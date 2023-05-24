data Auto = UnAuto{
    color :: String,
    velocidad :: Int,
    distancia :: Int,
    efectos :: [String]
} deriving  (Show, Eq)

-- Punto 1 --
estaCerca :: Auto -> Auto -> Bool
estaCerca a1 a2 
    | a1 == a2 = error "mismo auto"
    | abs(distancia a1 - distancia a2) < 10 = True
    | otherwise = False

vaTranquilo :: Auto -> [Auto] -> Bool
vaTranquilo auto autos 
    | estaLejos auto autos && vaPrimero auto autos = True
    | otherwise = False 

estaLejos :: Auto -> [Auto] -> Bool
estaLejos auto autos = not(any(estaCerca auto) autos)

vaPrimero :: Auto -> [Auto] -> Bool
vaPrimero auto autos = (maximum (map distancia autos)) < (distancia auto)

puesto :: Auto -> [Auto] -> Int
puesto auto autos = length(filter (>distancia auto) (map distancia autos)) + 1

-- Punto 2 -- 

correr :: Auto -> Int -> Auto
correr auto tiempo = UnAuto (color auto) (velocidad auto) (distancia auto + velocidad auto * tiempo) (efectos auto)

alterarVelocidad :: Int -> Int
alterarVelocidad a = div a 2

nuevaVelocidad :: Auto -> Int
nuevaVelocidad auto = alterarVelocidad (velocidad auto)

bajarVelocidad :: Auto -> Int
bajarVelocidad auto 
    | velocidad auto - (alterarVelocidad (velocidad auto)) > 0 = velocidad auto - (alterarVelocidad (velocidad auto))
    | otherwise = error "velocidad =  0"

-- Punto 3 --

data Carrera = UnaCarrera{ 
    power_up :: String,
    competidores :: [Auto]
}

terremoto :: Auto -> [Auto] -> [Auto]
terremoto auto autos =  map (disminuirVelocidad) (filter(estaCerca auto) autos) ++ filter (not.estaCerca auto) autos ++ [auto]

disminuirVelocidad :: Auto -> Auto
disminuirVelocidad auto = UnAuto (color auto) (velocidad auto - 50) (distancia auto) (efectos auto)

{-
miguelitos: este poder debe permitir configurarse con una cantidad que indica en cuánto deberán bajar la velocidad los autos que se vean afectados por su uso. Los autos a afectar son aquellos a los cuales el auto que gatilló el power up les vaya ganando.

-}

{-miguelitos ::  [Auto] -> Auto -> [Auto]
miguelitos  autos auto = filter (auto.distancia) (distancia autos)-}

vanDetras :: [Auto] -> Auto -> [Autos]
vanDetras autos auto 
    | length(filter (<distancia auto) (map distancia autos)) > 0 = 

afectarALosQueCumplen :: (a -> Bool) -> (a -> a) -> [a] -> [a]
afectarALosQueCumplen criterio efecto lista
  = (map efecto . filter criterio) lista ++ filter (not.criterio) lista

{-
jet pack: este poder debe afectar, dentro de la carrera, solamente al auto que gatilló el poder. El jet pack tiene un impacto que dura una cantidad limitada de tiempo, el cual se espera poder configurar.
Cuando se activa el poder del jet pack, el auto afectado duplica su velocidad actual, luego corre durante el tiempo indicado y finalmente su velocidad vuelve al valor que tenía antes de que se active el poder.
Por simplicidad, no se espera que los demás autos que participan de la carrera también avancen en ese tiempo.
-}

jetPack :: Auto -> Int -> Auto
jetPack auto tiempo
    | tiempo == 0 = UnAuto (color auto) (velocidad auto) (distancia auto) (efectos auto)
    | tiempo -1 >= 0 =    

-- Autos -- 
mario :: Auto
mario = UnAuto "rojo" 45 255 []
luigi :: Auto
luigi = UnAuto "verde" 23 250 []
donkyKong :: Auto
donkyKong = UnAuto "azul" 30 100 []