-- 1) Recursión sobre listas

-- 1.1) Dada una lista de enteros devuelve la suma de todos sus elementos.
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use foldr" #-}
{-# HLINT ignore "Use map" #-}
{-# HLINT ignore "Use camelCase" #-}
{-# HLINT ignore "Eta reduce" #-}
{-# HLINT ignore "Use newtype instead of data" #-}

sumatoria :: [Int] -> Int
sumatoria []     = 0
sumatoria (n:ns) = n + sumatoria ns

{- 1.2)Dada una lista de elementos de algún tipo devuelve el largo de esa 
lista, es decir, la cantidad de elementos que posee.
-}

longitud :: [a] -> Int
longitud []     = 0
longitud (n:ns) = 1 + longitud ns

-- 1.3)Dada una lista de enteros, devuelve la lista de los sucesores de cada entero.
sucesores :: [Int] -> [Int]
sucesores []     = []
sucesores (n:ns) = succ n : sucesores ns

-- 1.4)Dada una lista de booleanos devuelve True si todos sus elementos son True.
conjuncion :: [Bool] -> Bool
conjuncion []     = True
conjuncion (x:xs) = x && conjuncion xs

-- 1.5)Dada una lista de booleanos devuelve True si alguno de sus elementos es True.
disyuncion :: [Bool] -> Bool
disyuncion []     = False
disyuncion (x:xs) = x || disyuncion xs

-- 1.6) Dada una lista de listas, devuelve una única lista con todos sus elementos.

aplanar :: [[a]] -> [a]
aplanar []     = []
aplanar (x:xs) = x ++ aplanar xs

-- 1.7) Dados un elemento e y una lista xs devuelve True si existe un elemento en xs que sea igual a e.
pertenece :: Eq a => a -> [a] -> Bool
pertenece n [] = False
pertenece n (x:xs) = x == n || pertenece n xs


-- 1.8) Dados un elemento e y una lista xs cuenta la cantidad de apariciones de e en xs.
apariciones :: Eq a => a -> [a] -> Int
apariciones _ []     = 0
apariciones e (x:xs) =
    if e == x
        then 1 + apariciones e xs
        else apariciones e xs

-- 1.9) Dados un número n y una lista xs, devuelve todos los elementos de xs que son menores a n.
losMenoresA :: Int -> [Int] -> [Int]
losMenoresA _ []     = []
losMenoresA n (x:xs) =
    if n > x
        then x : losMenoresA n xs
        else losMenoresA n xs

-- 1.10) Dados un número n y una lista de listas, devuelve la lista de aquellas listas que tienen más de n elementos.
lasDeLongitudMayorA :: Int -> [[a]] -> [[a]]
lasDeLongitudMayorA _ []     = []
lasDeLongitudMayorA n (x:xs) =
    if length x > n
        then x : lasDeLongitudMayorA n xs
        else lasDeLongitudMayorA n xs

-- 1.11) Dados una lista y un elemento, devuelve una lista con ese elemento agregado al fínal de la lista.
agregarAlFinal :: [a] -> a -> [a]
agregarAlFinal [] n     = [n]
agregarAlFinal (x:xs) e = x : agregarAlFinal xs e

{- 1.12) Dadas dos listas devuelve la lista con todos los elementos de la primera lista y todos los
elementos de la segunda a continuación. Definida en Haskell como (++).
-}
agregar :: [a] -> [a] -> [a]
agregar [] ns     = ns
agregar (x:xs) ns = x : agregar xs ns

{- 1.13)Dada una lista devuelve la lista con los mismos elementos de atrás para adelante. Definida
en Haskell como reverse.
-}
reversa :: [a] -> [a]
reversa []     = []
reversa (x:xs) = reversa xs ++ [x]

{- 1.14) Dadas dos listas de enteros, devuelve una lista donde el elemento en la posición n es el
máximo entre el elemento n de la primera lista y de la segunda lista, teniendo en cuenta que
las listas no necesariamente tienen la misma longitud.
-}
zipMaximos :: [Int] -> [Int] -> [Int]
zipMaximos ns []         = ns
zipMaximos [] bs         = bs
zipMaximos (n:ns) (b:bs) = max n b : zipMaximos ns bs

-- 1.15) Dada una lista devuelve el mínimo.
--Precondición: La lista no puede ser vacía.
elMinimo :: Ord a => [a] -> a
elMinimo  [x]   = x
elMinimo (n:ns) = min n (elMinimo ns)

--2 Recursión sobre números

{- 2.1) Dado un número n se devuelve la multiplicación de este número y todos sus 
anteriores hasta llegar a 0. Si n es 0 devuelve 1. La función es parcial si n es negativo.
-}
--Precondición: n solo puede ser números enteros no negativos.
factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial (n - 1)

{- 2.2) Dado un número n devuelve una lista cuyos elementos sean los números comprendidos entre
n y 1 (incluidos). Si el número es inferior a 1, devuelve la lista vacía.
-}
cuentaRegresiva :: Int -> [Int]
cuentaRegresiva 0  = []
cuentaRegresiva n  =
    if (n < 1)
        then cuentaRegresiva 0
        else n : cuentaRegresiva (n - 1)

--2.3)Dado un número n y un elemento e devuelve una lista en la que el elemento e repite n veces.
--Precondición:  n no puede ser un número negativo.
repetir :: Int -> a -> [a]
repetir 0 _ = []
repetir n e = e : repetir (n - 1) e

{-2.4)Dados un número n y una lista xs, devuelve una lista con los n primeros elementos de xs.
Si la lista es vacía, devuelve una lista vacía.
--Precondición: n no puede ser un número negativo.
-}
losPrimeros :: Int -> [a] -> [a]
losPrimeros 0 _      = []
losPrimeros _ []     = []
losPrimeros n (x:xs) = x : losPrimeros (n - 1) xs

{-2.5)Dados un número n y una lista xs, devuelve una lista sin los primeros n elementos de lista
recibida. Si n es cero, devuelve la lista completa.
--Precondición: n no puede ser un número negativo.
-}
sinLosPrimeros :: Int -> [a] -> [a]
sinLosPrimeros 0 xs     = xs
sinLosPrimeros _ []     = []
sinLosPrimeros n (_:xs) = sinLosPrimeros (n - 1) xs


--3) Registros

--3.1)
data Persona = ConsPersona String Int deriving Show

-- Dados una edad y una lista de personas devuelve a las personas mayores a esa edad.
mayoresA :: Int -> [Persona] -> [Persona]
mayoresA _ []            = []
mayoresA edadMin (p : ps) =
    if esMayor p edadMin
        then p : mayoresA edadMin ps
        else mayoresA edadMin ps

esMayor :: Persona -> Int -> Bool
esMayor (ConsPersona _ a) b = a > b

--Lista de personas para probar
personas :: [Persona]
personas = [ConsPersona "Carlos" 40, ConsPersona "Maria" 22, ConsPersona "Miguel" 50, ConsPersona "Laura" 16]


--Dada una lista de personas devuelve el promedio de edad entre esas personas.
--Precondición: la lista al menos posee una persona.
promedioEdad :: [Persona] -> Int
promedioEdad [] = error "La lista no puede ser vacia"
promedioEdad xs = div (sumaDeEdades xs) (length xs)

sumaDeEdades :: [Persona] -> Int
sumaDeEdades [] = 0
sumaDeEdades (p : ps) = edadDe p + sumaDeEdades ps

edadDe :: Persona -> Int 
edadDe (ConsPersona _ e) = e

--Dada una lista de personas devuelve la persona más vieja de la lista. 
--Precondición: la lista al menos posee una persona.
elMasViejo :: [Persona] -> Persona
elMasViejo [alguien] = alguien
elMasViejo (p : ps)  =
    if esMayorQueLaOtra p (elMasViejo ps)
        then p
        else elMasViejo ps



esMayorQueLaOtra :: Persona -> Persona -> Bool
esMayorQueLaOtra x b = edad x > edad b

edad :: Persona -> Int
edad (ConsPersona n b) = b

--3.2)Modificaremos la representación de Entreador y Pokemon de la práctica anterior de la siguiente manera:
data TipoDePokemon = Agua | Fuego | Planta           deriving Show
data Pokemon       = ConsPokemon TipoDePokemon Int   deriving Show
data Entrenador    = ConsEntrenador String [Pokemon] deriving Show

--Devuelve la cantidad de Pokémon que posee el entrenador.
cantPokemon :: Entrenador -> Int
cantPokemon (ConsEntrenador _ pokeList) = length pokeList


pepo = ConsEntrenador "pepo" [ConsPokemon Agua 30, ConsPokemon Fuego 25, ConsPokemon Planta 44] --Entrenadora para probar

--Devuelve la cantidad de Pokémon de determinado tipo que posee el entrenador.
cantPokemonDe :: TipoDePokemon -> Entrenador -> Int
cantPokemonDe tipo (ConsEntrenador _ listaPokemon) = contarTipo tipo listaPokemon

contarTipo :: TipoDePokemon -> [Pokemon] -> Int
contarTipo _ [] = 0
contarTipo tipo (p : xs) =
    if sonIguales tipo (elTipo p)
        then 1 + contarTipo tipo xs
        else contarTipo tipo xs

elTipo :: Pokemon -> TipoDePokemon 
elTipo (ConsPokemon a _) = a

sonIguales :: TipoDePokemon -> TipoDePokemon -> Bool
sonIguales Agua Agua = True
sonIguales Fuego Fuego = True
sonIguales Planta Planta = True
sonIguales _ _ = False


nati = ConsEntrenador "nati" [ConsPokemon Agua 2, ConsPokemon Fuego 1, ConsPokemon Agua 1] -- entrenadora para probar


--Dados dos entrenadores, indica la cantidad de Pokemon de cierto tipo, que le ganarían
--a los Pokemon del segundo entrenador.
cuantosDeTipo_De_LeGananATodosLosDe_ :: TipoDePokemon -> Entrenador -> Entrenador -> Int
cuantosDeTipo_De_LeGananATodosLosDe_ tp (ConsEntrenador _ ps) e =  cuantosDeTipoLeGananA ps tp e

cuantosDeTipoLeGananA :: [Pokemon] -> TipoDePokemon -> Entrenador -> Int
cuantosDeTipoLeGananA [] _ _= 0
cuantosDeTipoLeGananA (p : ps) tp e = 
    if esDeTipo p tp && superaATodos p (pokemonesDe e)
      then 1 + cuantosDeTipoLeGananA ps tp e
      else cuantosDeTipoLeGananA ps tp e

pokemonesDe :: Entrenador -> [Pokemon] 
pokemonesDe (ConsEntrenador _ ps) = ps

esDeTipo :: Pokemon -> TipoDePokemon -> Bool
esDeTipo (ConsPokemon tp1 _) tp2 = sonMismoTipo tp1 tp2

superaATodos :: Pokemon -> [Pokemon] -> Bool
superaATodos _ [] = True
superaATodos p (n:ns) = superaA p n && superaATodos p ns

superaA :: Pokemon -> Pokemon -> Bool
superaA (ConsPokemon t1 e1) (ConsPokemon t2 e2) = superaATipo t1 t2

superaATipo :: TipoDePokemon -> TipoDePokemon -> Bool
superaATipo Agua Fuego = True
superaATipo Fuego Planta = True
superaATipo Planta Agua = True
superaATipo _ _ = False

sonMismoTipo :: TipoDePokemon -> TipoDePokemon -> Bool
sonMismoTipo Agua Agua = True
sonMismoTipo Fuego Fuego = True
sonMismoTipo Planta Planta = True
sonMismoTipo _ _ = False



-- Dado un entrenador, devuelve True si posee al menos un Pokémon de cada tipo posible.
esMaestroPokemon :: Entrenador -> Bool
esMaestroPokemon (ConsEntrenador _ pokemones) = tieneTipo Agua pokemones && tieneTipo Fuego pokemones && tieneTipo Planta pokemones

tieneTipo :: TipoDePokemon -> [Pokemon] -> Bool
tieneTipo _ [] = False
tieneTipo tipo (ConsPokemon otroTipo _ : ps) = sonMismoTipo tipo otroTipo || tieneTipo tipo ps


-- lista pokemon
charmander = ConsPokemon Fuego 26
odish      = ConsPokemon Planta 25
palkia     = ConsPokemon Agua 24
rapidash   = ConsPokemon Fuego 33

xime = ConsEntrenador "xime" [charmander, odish, palkia] -- entrenador con todos los tipos
chalu = ConsEntrenador "chalu" [odish, palkia]            -- entrenador con dos tipos
sami = ConsEntrenador "sami" [charmander, rapidash]


--3.3)
data Seniority = Junior | SemiSenior | Senior                             deriving Show
data Proyecto = ConsProyecto String                                       deriving Show
data Rol = Developer Seniority Proyecto | Management Seniority Proyecto   deriving Show
data Empresa = ConsEmpresa [Rol]                                          deriving Show

-- Dada una empresa denota la lista de proyectos en los que trabaja, sin elementos repetidos.
proyectos :: Empresa -> [Proyecto]
proyectos (ConsEmpresa ns) = proyectosDe ns

proyectosDe :: [Rol] -> [Proyecto]
proyectosDe []     = []
proyectosDe (n:ns) =
     if incluyeProyecto (proyectoDe_ n) (proyectosDe ns)
        then proyectosDe ns
        else proyectoDe_ n : proyectosDe ns 

proyectoDe_ :: Rol -> Proyecto
proyectoDe_ (Developer _ n) = n
proyectoDe_ (Management _ n) = n

incluyeProyecto :: Proyecto -> [Proyecto] -> Bool
incluyeProyecto _ []     = False
incluyeProyecto p (n:ns) = esMismoProyecto p n || incluyeProyecto p ns

esMismoProyecto :: Proyecto -> Proyecto -> Bool
esMismoProyecto (ConsProyecto n) (ConsProyecto p)= n == p

--Dada una empresa indica la cantidad de desarrolladores senior que posee, que pertecen
--además a los proyectos dados por parámetro.
losDevSenior :: Empresa -> [Proyecto] -> Int
losDevSenior (ConsEmpresa xs) ps = cantidadDevSenior xs ps 

cantidadDevSenior :: [Rol] -> [Proyecto] -> Int
cantidadDevSenior [] _        = 0
cantidadDevSenior (x : xs) ps =
     if esDevSenior x ps
        then 1 + cantidadDevSenior xs ps
        else cantidadDevSenior xs ps

esDevSenior :: Rol -> [Proyecto] -> Bool
esDevSenior (Developer s p) ps  = esSenior s && incluyeProyecto p ps
esDevSenior _ _                 = False

esSenior :: Seniority -> Bool
esSenior Senior = True
esSenior _      = False

-- Indica la cantidad de empleados que trabajan en alguno de los proyectos dados.
cantQueTrabajanEn :: [Proyecto] -> Empresa -> Int
cantQueTrabajanEn [] _                    = 0
cantQueTrabajanEn (p:ps) (ConsEmpresa rs) = cantidadQueTrabajanEn p rs + cantQueTrabajanEn ps (ConsEmpresa rs)

cantidadQueTrabajanEn :: Proyecto -> [Rol] -> Int
cantidadQueTrabajanEn _ [] = 0
cantidadQueTrabajanEn p (x:xs) =
    if  esMismoProyecto p (proyectoDe_ x)
        then 1 + cantidadQueTrabajanEn p xs
        else cantidadQueTrabajanEn p xs



-- Devuelve una lista de pares que representa a los proyectos (sin repetir) junto con su
-- cantidad de personas involucradas.
asignadosPorProyecto :: Empresa -> [(Proyecto, Int)]
asignadosPorProyecto (ConsEmpresa rs) = cantidadDeAsignados rs (proyectosDe rs)

cantidadDeAsignados :: [Rol] -> [Proyecto] -> [(Proyecto, Int)]
cantidadDeAsignados     _   [] = []
cantidadDeAsignados rs  (p:ps) = (p, cantidadQueTrabajanEn p rs) : cantidadDeAsignados rs ps


