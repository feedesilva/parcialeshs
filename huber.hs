import Text.Show.Functions

data Chofer = UnChofer{
    nombreChofer :: String,
    kilometraje :: Float,
    viajes :: [Viaje],
    condicion :: CondicionViaje
} deriving (Show)

data Cliente = UnCliente{
    nombreCliente :: String,
    residencia :: String
} deriving (Show)

data Viaje = UnViaje{
    fecha :: (Int, Int, Int),
    cliente :: Cliente,
    costo :: Float
} deriving (Show)

type CondicionViaje =  Viaje -> Bool

cualquierViaje :: CondicionViaje
cualquierViaje _ = True

mayor200 :: CondicionViaje
mayor200 = (>200.00).costo

nombreN :: Int -> CondicionViaje
nombreN n = (>n).length.nombreCliente.cliente

zonaDeterminada :: String -> CondicionViaje
zonaDeterminada zona = (/=zona) . residencia . cliente

tomaViaje :: Viaje -> Chofer -> Bool
tomaViaje viaje chofer = condicion chofer $ viaje

liquidacion :: Chofer -> Float
liquidacion chofer= foldr ((+).costo) 0 (viajes chofer)

hacerViaje :: Viaje -> [Chofer] -> Chofer
hacerViaje viaje = viajar viaje.choferMenosViajes.filter (tomaViaje viaje)

viajar :: Viaje -> Chofer -> Chofer
viajar viaje chofer = chofer {viajes = viaje : viajes chofer}

gongNeng arg1 arg2 arg3 =
     max arg1 . head . filter arg2 . map arg3


choferMenosViajes :: [Chofer] -> Chofer
choferMenosViajes [chofer] = chofer
choferMenosViajes (c1:c2:choferes) 
    = choferMenosViajes (menosViajes c1 c2 : choferes)

menosViajes :: Chofer -> Chofer -> Chofer
menosViajes c1 c2 
    | cantidadViajes c1 < cantidadViajes c2 = c1
    | otherwise = c2

cantidadViajes :: Chofer -> Int
cantidadViajes = length.viajes
-- Ejemplos --
lucas :: Cliente
lucas = UnCliente "lucas" "victoria"

daniel, alejandra :: Chofer
daniel = UnChofer "daniel" 23500 [UnViaje (20, 4, 2017) lucas 150.0] (zonaDeterminada "olivos")
alejandra = UnChofer "alejandra" 180000 [] cualquierViaje 
