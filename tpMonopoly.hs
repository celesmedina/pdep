main :: IO ()    -- This says that main is an IO action.
main = return () 

data Participante=Participante String Int String [(String, Int)] [String] deriving (Show)

carolina=Participante  "Carolina" 500 "Accionista" [] ["pagarAAccionistas"]
manuel=Participante  "Manuel" 500 "Oferente singular" [] ["pasarPorElBanco", "enojarse"]


pasarPorElBanco::Participante->Participante
pasarPorElBanco (Participante nombre dinero tactica propiedades acciones)=Participante nombre (dinero+40) "Comprador compulsivo" propiedades acciones

enojarse::Participante->Participante
enojarse (Participante nombre dinero tactica propiedades acciones)=Participante nombre (dinero+50) tactica propiedades ("gritar":acciones)

gritar::Participante->Participante
gritar (Participante nombre dinero tactica propiedades acciones)=Participante ("AHHHH"++nombre) dinero tactica propiedades acciones

subastar::Participante->(String,Int)->Participante
subastar (Participante nombre dinero tactica propiedades acciones) (nombrePropiedad, precio)| tactica=="Oferente singular" ||tactica=="Accionista"= Participante nombre (dinero-precio) tactica ((nombrePropiedad, precio):propiedades) acciones
                                                                                            |otherwise=Participante nombre dinero tactica propiedades acciones

esBarata::(String,Int)->Bool
esBarata (_,precio)=precio<150

sumarAlquileresBaratos::[(String,Int)]->Int
sumarAlquileresBaratos propiedades= length(filter esBarata propiedades)*10

sumarAlquileresCaros::[(String,Int)]->Int
sumarAlquileresCaros propiedades=length (filter (not.esBarata) propiedades)*20

cobrarAlquileres::Participante->Participante
cobrarAlquileres (Participante nombre dinero tactica propiedades acciones)= Participante nombre (dinero+(sumarAlquileresCaros propiedades +sumarAlquileresBaratos propiedades)) tactica propiedades acciones   

pagarAAccionistas::Participante->Participante
pagarAAccionistas (Participante nombre dinero tactica propiedades acciones)|tactica=="Accionista"=Participante nombre (dinero+200) tactica propiedades acciones 
                                                                           | otherwise=Participante nombre (dinero-100) tactica propiedades acciones