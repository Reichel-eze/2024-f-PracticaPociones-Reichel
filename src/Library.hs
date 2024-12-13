module Library where
import PdePreludat

data Persona = Persona {
    nombrePersona :: String,
    suerte :: Number,
    inteligencia :: Number,
    fuerza :: Number
} deriving (Show, Eq)

data Pocion = Pocion {
    nombrePocion :: String,
    ingredientes :: [Ingrediente]
}

type Efecto = Persona -> Persona

data Ingrediente = Ingrediente {
    nombreIngrediente :: String,
    efectos :: [Efecto]
}

nombresDeIngredientesProhibidos = [
    "sangre de unicornio",
    "veneno de basilisco",
    "patas de cabra",
    "efedrina"]

maximoSegun :: Ord b => (a -> b) -> [a] -> a
maximoSegun _ [ x ] = x
maximoSegun  f ( x : y : xs)
    | f x > f y = maximoSegun f (x:xs)
    | otherwise = maximoSegun f (y:xs)

-- 1) Dada una persona definir las siguientes funciones para cuantificar sus 
-- niveles de suerte, inteligencia y fuerza sin repetir código:

-- Haciendo una lista de niveles pierdo la info la categoria del nivel (que en este caso no la necesito)
-- Pero me ahorra la repeticion de logica a la hora de consultar sobre una lista
-- ES MUCHO MAS FACIL OPERAR SOBRE LISTAS
niveles :: Persona -> [Number]
niveles persona = [suerte persona, inteligencia persona,  fuerza persona]

-- 1.a) Definir sumaDeNiveles que suma todos sus niveles.

-- CON REPETICION DE LOGICA
--sumaDeNiveles :: Persona -> Number
--sumaDeNiveles persona = suerte persona + inteligencia persona + fuerza persona

-- SIN REPETICION DE LOGICA
sumaDeNiveles :: Persona -> Number
sumaDeNiveles = sum . niveles 

-- 1.b) Definir diferenciaDeNiveles es la diferencia entre el nivel más alto y más bajo.

-- CON REPETICION DE LOGICA
--diferenciaDeNiveles :: Persona -> Number
--diferenciaDeNiveles persona = maximoNivel persona - minimoNivel persona

--maximoNivel :: Persona -> Number
--maximoNivel persona = suerte persona `max` inteligencia persona `max` fuerza persona

--minimoNivel :: Persona -> Number
--minimoNivel persona = suerte persona `min` inteligencia persona `min` fuerza persona

-- SIN REPETICION DE LOGICA
diferenciaDeNiveles :: Persona -> Number
diferenciaDeNiveles persona = maximoNivel persona - minimoNivel persona

maximoNivel :: Persona -> Number
maximoNivel = maximum . niveles

minimoNivel :: Persona -> Number
minimoNivel = minimum . niveles

-- 1.c) Definir nivelesMayoresA n, que indica la cantidad de niveles de la persona que están por encima del valor dado.

nivelesMayoresA :: Number -> Persona -> Number
nivelesMayoresA n = length . filter (> n) . niveles
-- 1ero. Obtengo la lista de niveles de la persona
-- 2dos. Me quedo con aquellos que sean mayores al valor pasado por parametro
-- 3ero. Cuento la cantidad de aquellos niveles que obtuve luego del filtrado