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

-- 2) Definir la función efectosDePocion que dada una poción devuelve una lista con los efectos de todos sus ingredientes.

efectosDePosion :: Pocion -> [Efecto]
efectosDePosion =  concatMap efectos . ingredientes
-- 1ero. Necesito la lista de ingredientes de la pocion
-- 2dos. De cada ingrediente necesito la lista de efectos --> Obtengo una lista de listas de efectos
-- 3ero. Aplano dichas listas en una sola lista de efectos

-- 3) Dada una lista de pociones, consultar:

-- 3.a) Los nombres de las pociones hardcore, que son las que tienen al menos 4 efectos.

pocionesHardcore :: [Pocion] -> [String]
pocionesHardcore = map nombrePocion . filter esPocionHardcore
-- 1ero. Filtro las pociones que son hardocore
-- 2dos. Luego transformo dicha lista filtrada en una lista de los nombres de las pociones

esPocionHardcore :: Pocion -> Bool
esPocionHardcore = (>=4) . length . efectosDePosion

-- 3.b) La cantidad de pociones prohibidas, que son aquellas que tienen algún ingrediente cuyo nombre figura en 
-- la lista de ingredientes prohibidos.
 
cantidadPocionesProhibidas :: [Pocion] -> Number
cantidadPocionesProhibidas = length . filter esPocionProhibida

esPocionProhibida :: Pocion -> Bool
esPocionProhibida = any ( (`elem` nombresDeIngredientesProhibidos) . nombreIngrediente) . ingredientes

--esPocionProhibida :: Pocion -> Bool
--esPocionProhibida = any (\ingre -> nombreIngrediente ingre `elem` nombresDeIngredientesProhibidos). ingredientes

esIngredienteProhibido :: Ingrediente -> Bool
esIngredienteProhibido = (`elem ` nombresDeIngredientesProhibidos) . nombreIngrediente

-- 3.c) Si son todas dulces, lo cual ocurre cuando todas las pociones de la lista tienen algún ingrediente llamado “azúcar”.

sonTodasDulces :: [Pocion] -> Bool
sonTodasDulces = all tieneAzucar 

tieneAzucar :: Pocion -> Bool
tieneAzucar = any ((== "azucar") . nombreIngrediente) . ingredientes

--tieneAzucar :: Pocion -> Bool
--tieneAzucar = any (\ingre -> nombreIngrediente ingre == "azucar") . ingredientes

-- 4) Definir la función tomarPocion que recibe una poción y una persona, y devuelve como quedaría la persona después 
-- de tomar la poción. Cuando una persona toma una poción, se aplican todos los efectos de esta última, en orden.

tomarPocion :: Pocion -> Persona -> Persona
tomarPocion pocion personaInicial = foldl           aplicarEfecto              personaInicial (efectosDePosion pocion)
--                                        (\persona efecto -> efecto persona)    "semilla"      "lista de efectos"

-- funcion de reduccion
aplicarEfecto :: Persona -> Efecto -> Persona
aplicarEfecto persona efecto = efecto persona

-- 5) Definir la función esAntidotoDe que recibe dos pociones y una persona, y dice si tomar la segunda 
--  poción revierte los cambios que se producen en la persona al tomar la primera.

esAntidotoDe :: Pocion -> Pocion -> Persona -> Bool
esAntidotoDe pocion antidoto persona = ((== persona) . tomarPocion antidoto . tomarPocion pocion) persona
-- 1ero. La persona se toma la pocion --> Devuelve una persona (una nueva persona si tomara la pocion) (Porque NO HAY EFECTO!!)
-- 2dos. La persona que me devuelve al tomar la pocion ahora la hago tomar el antidoto
-- 3ero. Consulto si la persona resultante al tomar el antidoto es igual a la persona que tenia en un comienzo

-- 6) Definir la función personaMasAfectada que recibe una poción, una función cuantificadora 
-- (es decir, una función que dada una persona retorna un número) y una lista de personas, y devuelve a la 
-- persona de la lista que hace máxima el valor del cuantificador. 
-- Mostrar un ejemplo de uso utilizando los cuantificadores definidos en el punto 1.

personaMasAfectada :: Pocion -> (Persona -> Number) -> [Persona] -> Persona
personaMasAfectada pocion criterio = maximoSegun criterio . map (tomarPocion pocion)
-- 1ero. Obtengo una lista de las personas luego de tomar la pocion
-- 2dos. Obtengo la persona que es la que mas (segun la funcion cuantificadora) afectada es de esa lista

personaMasAfectada' :: Pocion -> (Persona -> Number) -> [Persona] -> Persona
personaMasAfectada' pocion criterio = maximoSegun (criterio . tomarPocion pocion)

-- funcionesCuantificadoras (criterios) definidos en el punto 1):
-- sumaDeNiveles :: Persona -> Number
-- diferenciaDeNiveles :: Persona -> Number
-- nivelesMayoresA :: Number -> Persona -> Number

-- Ejemplitos
ejemplo1 = personaMasAfectada (Pocion "placebo" []) sumaDeNiveles []
ejemplo2 = personaMasAfectada (Pocion "placebo" []) diferenciaDeNiveles []
ejemplo3 = personaMasAfectada (Pocion "placebo" []) (nivelesMayoresA 10) []
