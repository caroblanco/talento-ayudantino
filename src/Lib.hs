module Lib where
import Text.Show.Functions
import Data.List

data Ayudante = UnAyudante {
    nombreA :: String,
    conceptos :: [(String, Int)]
} deriving (Show,Eq)

obtenerListaNiveles :: Ayudante -> [Int]
obtenerListaNiveles = map snd .conceptos

obtenerListaConceptos :: Ayudante -> [String]
obtenerListaConceptos = map fst. conceptos
--------1
guille :: Ayudante
guille = UnAyudante {nombreA = "Guille", conceptos = [("orden superior",6),("expresiones lamda",7),("fold",8)]}

elChacal :: Ayudante
elChacal = UnAyudante {nombreA = "El Chacal", conceptos = [("aplicacion parcial",9),("sinonimos de tipo",7),("fold",6)]}

vicky :: Ayudante
vicky = UnAyudante {nombreA = "Vicky", conceptos = [("orden superior",8),("aplicacion parcial",8),("clases de tipo",5), ("tuplas", 9)]}

-------2
cuantosTienenEseNivel :: Int -> [Ayudante] -> Int
cuantosTienenEseNivel nivel = length . filter (condicionNivel nivel)

condicionNivel :: Int -> Ayudante -> Bool
condicionNivel nivel = any ((==nivel).snd).conceptos

----------3
aprendioCadaVezMas :: Ayudante -> Bool
aprendioCadaVezMas ayudante = sort (obtenerListaNiveles ayudante) == obtenerListaNiveles ayudante

----------1
type Jurado = [Juez]
type Juez = Ayudante->Int

gise :: Juez
gise ayudante = div (sum (obtenerListaNiveles ayudante)) (length (obtenerListaNiveles ayudante))

marche :: Juez
marche ayudante 
    | sabeTalConcepto "orden superior" ayudante = 9
    | otherwise = 5

sabeTalConcepto :: String -> Ayudante -> Bool
sabeTalConcepto concepto = elem concepto .obtenerListaConceptos

hernan :: String -> Juez
hernan "buen dia" ayudante = puntajeHernan ayudante + 2
hernan _ ayudante = puntajeHernan ayudante

puntajeHernan :: Ayudante -> Int
puntajeHernan = length. obtenerListaConceptos

--------2
daPuntaje :: Juez -> Ayudante -> Int
daPuntaje juez = juez

promedioPuntajes :: Jurado -> Ayudante -> Int
promedioPuntajes jurado ayudante = div (otorgarPuntajesJurado ayudante jurado) (length jurado)

otorgarPuntajesJurado ::  Ayudante -> Jurado -> Int
otorgarPuntajesJurado ayudante = sum.map (`daPuntaje` ayudante)

---------3
esBuenAyudante :: Ayudante -> Jurado -> Bool
esBuenAyudante ayudante  = (>=7).otorgarPuntajesJurado ayudante

---------4
juecesEj = [gise, marche , hernan "buen dia", (\ayudante -> maximum (obtenerListaNiveles ayudante))]
--promedioPuntajes juecesEj guille
--7

---------0
type Criterio  = Ayudante -> Int

maximoAyudanteSegun :: (Ayudante -> Int) -> [Ayudante] -> Ayudante
maximoAyudanteSegun criterio lista = foldl1 (maximo criterio) lista

maximo criterio unAyudante otroAyudante | criterio unAyudante > criterio otroAyudante = unAyudante
                                        | otherwise = otroAyudante

maximoAyudanteSegun2 criterio ayudantes = head (primeros3 criterio ayudantes)
----------1
primeros3 ::  Criterio -> [Ayudante] -> [Ayudante]
primeros3 criterio = take 3 .  ordenarSegunCriterio criterio

ordenarSegunCriterio :: (Ayudante->Int) -> [Ayudante] -> [Ayudante] 
ordenarSegunCriterio _ [] = []
ordenarSegunCriterio criterio (ayudante:resto) = aux criterio ayudante resto []

aux criterio primero [] r = primero : ordenarSegunCriterio criterio r
aux criterio primero (segundo:resto) r
    | criterio segundo > criterio primero = aux criterio segundo resto (primero:r)
    | otherwise = aux criterio primero resto (segundo:r) 

{- ordenarSegunCriterio criterio (x:xs) = aux criterio xs x []

aux criterio [] m r = m : ordenarSegunCriterio criterio r
aux criterio (x:xs) m r 
    | criterio x > criterio m     = aux criterio xs x (m:r)
    | otherwise = aux criterio xs m (x:r) -}

-----a
temasMayorA7 :: Criterio
temasMayorA7 ayudante = length (filter (>7) (obtenerListaNiveles ayudante))
-- primeros3 maximosPoMayorA7 [guille,vicky]
-- UnAyudante {nombre = "Vicky", conceptosParadigmas = [("clasesDeTipo",5),("aplicacionParcial",8),("tuplas",9),("ordenSuperior",8)]}
-----------b
segunJuez :: Juez -> Criterio
segunJuez juez = daPuntaje juez
-- primeros3 maximosPorJuez gise [guille,vicky]
-- UnAyudante {nombre = "vicky", conceptosParadigmas = [("clasesDeTipo",5),("aplicacionParcial",8),("tuplas",9),("ordenSuperior",8)]}
---------c
notaEnTalTema :: String -> Criterio
notaEnTalTema tema ayudante
    |sabeTalConcepto tema ayudante = notaTema tema (conceptos ayudante)
    |otherwise = 0

notaTema :: String ->  [(String,Int)] -> Int
notaTema tema conceptos = snd (head (filter ((==tema).fst) conceptos)) 

-- primeros3 notaEnTalTema [guille,vicky]
-- [UnAyudante {nombre = "Guille", conceptosParadigmas = [("ordenSuperior",6),("expresionLambda",7),("fold",8)]},UnAyudante {nombre = "Vicky", conceptosParadigmas = [("clasesDeTipo",5),("aplicacionParcial",8),("tuplas",9),("ordenSuperior",8)]}]

----------3
{-
Si la lista de conocimientos fuese infinita no se pobria llegar a un resultado en el punto a porque 
nunca se podria terminar de evaluar la condicion y llegar a un resultado.
en el b, depende del juez. para gise es idem a a, para marche podria si en algun lugar de la lista tiene 
"orden superior", sino tampoco nunca temrina, y para hernan idem a.
para el c, solo terminaria si encuentra al concepto, sino no. 
-}