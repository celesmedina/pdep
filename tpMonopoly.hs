import Text.Show.Functions

main :: IO ()
main = return ()

type Propiedad = (String, Int)
type Accion = Participante -> Participante

data Participante = Participante {
  nombre::String,
  dinero::Int,
  tactica::String,
  propiedades::[ Propiedad ],
  acciones::[ Accion ]
} deriving Show

sumarDinero monto participante = participante { dinero = (dinero participante) + monto }

pasarPorElBanco :: Participante -> Participante
pasarPorElBanco (Participante nombre dinero tactica propiedades acciones) = Participante nombre (dinero + 40) "Comprador compulsivo" propiedades acciones

enojarse :: Participante -> Participante
enojarse (Participante nombre dinero tactica propiedades acciones) = Participante nombre (dinero + 50) tactica propiedades (acciones ++ [gritar])

gritar :: Participante -> Participante
gritar participante = participante {
  nombre = ("AHHHH" ++ nombre participante)
}

esTacticaGanadora :: String -> Bool
esTacticaGanadora "Oferente singular" = True
esTacticaGanadora "Accionista" = True
esTacticaGanadora _ = False

adquirirPropiedad :: Propiedad -> Accion
adquirirPropiedad propiedad persona
  = persona {
    propiedades = (propiedad : propiedades persona),
    dinero = (dinero persona - snd propiedad)
  }

subastar :: Propiedad -> Accion
subastar propiedad persona
  | esTacticaGanadora . tactica $ persona = adquirirPropiedad propiedad persona
  | otherwise=persona

esBarata :: Propiedad -> Bool
esBarata (_, precio) = precio < 150

calcularValorPropiedad :: Propiedad -> Int
calcularValorPropiedad propiedad
  | esBarata propiedad = 10
  | otherwise = 20

sumarAlquileresPropiedades :: [Propiedad]->Int
sumarAlquileresPropiedades propiedades = sum (map calcularValorPropiedad propiedades)

sumarAlquileres :: Participante -> Int
sumarAlquileres participante = sumarAlquileresPropiedades (propiedades participante)

cobrarAlquileres :: Participante -> Participante
cobrarAlquileres participante = sumarDinero (sumarAlquileres participante) participante

pagarAAccionistas :: Participante -> Participante
pagarAAccionistas participante
  | tactica participante == "Accionista"  = sumarDinero 200 participante
  | otherwise                             = sumarDinero (-100) participante

hacerBerrinchePor :: Propiedad -> Accion
hacerBerrinchePor propiedad participante
  | snd propiedad <= dinero participante  = adquirirPropiedad propiedad participante
  | otherwise                             = hacerBerrinchePor propiedad (gritar (sumarDinero 10 participante))

ultimaRonda :: Participante -> Participante
ultimaRonda participante
  = foldl (\p fn -> fn p) participante (acciones participante)

juegoFinal :: Participante -> Participante -> Participante
juegoFinal participante1 participante2
  | dinero (ultimaRonda participante1) > dinero (ultimaRonda participante2) = ultimaRonda participante1
  | otherwise = ultimaRonda participante2

-- Modelar a Carolina y Manuel
carolina :: Participante
manuel :: Participante
carolina = Participante  "Carolina" 500 "Accionista" [] [pasarPorElBanco, pagarAAccionistas]
manuel = Participante  "Manuel" 500 "Oferente singular" [] [pasarPorElBanco, enojarse]