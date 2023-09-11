-- ejercicio 1  Consejos

{-Recordar probar las funciones definidas con ejemplos.
  Recordar determinar las precondiciones de las funciones que se definen.
  Recordar analizar la calidad de las soluciones, controlando:
    - que la cantidad de casos sea razonable;
    - que la división en subtareas sea adecuada;
    - que no haya vicios como "miedo al booleano";
    - que los nombres de las subtareas sean descriptivos y significativos.
-}

-- ejercicio 2; 1)Numeros enteros
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use list literal" #-}
{-# HLINT ignore "Use max" #-}
-- A
sucesor :: Int -> Int
sucesor x = x + 1

-- B
sumar :: Int -> Int -> Int
sumar n m = n + m

--C
-- Precondición: el segundo argumento no puede ser 0
divisionYResto :: Int -> Int -> (Int, Int)
divisionYResto n m = (div n m, mod n m)

--D
maxDelPar :: (Int,Int) -> Int
maxDelPar (x, y) =
  if x > y
     then x
     else y


-- ejercicio 2; 2)Numeros enteros

ejemplo1 = sumar (maxDelPar (divisionYResto (sucesor 19) (sucesor 1))) 0

ejemplo2 = maxDelPar (divisionYResto (sucesor 9) (sumar 0 1))

ejemplo3 = sucesor (maxDelPar (divisionYResto (sumar 2 7) 10))

ejemplo4 = sumar (maxDelPar (divisionYResto 10 2)) (sucesor 4)

-- ejercicio 3; 1)
data Dir = Norte | Este | Sur | Oeste
    deriving Show
--a
opuesto :: Dir -> Dir
opuesto Norte = Sur
opuesto Sur = Norte
opuesto Este = Oeste
opuesto Oeste= Este

--b
iguales :: Dir -> Dir -> Bool
iguales Norte Norte = True
iguales Este Este = True
iguales Sur Sur = True
iguales Oeste Oeste = True
iguales _ _ = False

--c
-- Precondición: No hay otra dirección despues de Oeste.
-- Es una función parcial porque si se le proporciona una entrada que no pertenece a su dominio válido puede fallar.
siguiente :: Dir -> Dir
siguiente Norte = Este
siguiente Este = Sur
siguiente Sur = Oeste
siguiente Oeste = error "No hay otra"
-- ejercicio 3; 2)
data DiaDeSemana = Lunes | Martes | Miercoles | Jueves | Viernes | Sabado | Domingo
    deriving Show
--a
primeroYUltimoDia :: (DiaDeSemana, DiaDeSemana)
primeroYUltimoDia = (primerDiaDeSemana, ultimoDiaDeSemana)

primerDiaDeSemana :: DiaDeSemana
primerDiaDeSemana = Lunes

ultimoDiaDeSemana :: DiaDeSemana
ultimoDiaDeSemana = Domingo

--b
empiezaConM :: DiaDeSemana -> Bool
empiezaConM Martes = True
empiezaConM Miercoles = True
empiezaConM _ = False

--c
vieneDespues :: DiaDeSemana -> DiaDeSemana -> Bool
vieneDespues Lunes Domingo = True
vieneDespues Martes Lunes = True
vieneDespues Miercoles Martes = True
vieneDespues Jueves Miercoles = True
vieneDespues Viernes Jueves = True
vieneDespues Sabado Viernes = True
vieneDespues Domingo Sabado = True
vieneDespues _ _ = False

--d
estaEnElMedio :: DiaDeSemana -> Bool
estaEnElMedio Lunes = False
estaEnElMedio Domingo = False
estaEnElMedio _ = True

-- ejercicio 3; 3)

--a
negar :: Bool -> Bool
negar True = False
negar _    = True

--b
implica :: Bool -> Bool -> Bool
implica True a = a
implica _ _    = True

--c
yTambien :: Bool -> Bool -> Bool
yTambien True a = a
yTambien _ _    = False

--d
oBien :: Bool -> Bool -> Bool
oBien False a = a
oBien a _     = a


-- ejercicio 4; 1)
data Persona = ConsP String Int
    deriving Show

--Devuelve el nombre de una persona
nombre :: Persona -> String
nombre (ConsP n b) = n

--Devuelve la edad de una persona
edad :: Persona -> Int
edad (ConsP n b) = b

--Aumenta en uno la edad de la persona
crecer :: Persona -> Persona
crecer (ConsP n d) = ConsP n (edadMasUno d)

edadMasUno :: Int -> Int
edadMasUno d = d + 1

{-Dados un nombre y una persona, devuelve una persona 
con la edad de la persona y el nuevo nombre.
-}
cambioDeNombre :: String -> Persona -> Persona
cambioDeNombre nn (ConsP n e) = ConsP nn e

--Dadas dos personas indica si la primera es mayor que la segunda
esMayorQueLaOtra :: Persona -> Persona -> Bool
esMayorQueLaOtra x b = edad x > edad b

--Dadas dos personas devuelve a la persona que sea mayor
laQueEsMayor :: Persona -> Persona -> Persona
laQueEsMayor x b = if esMayorQueLaOtra x b
                                   then x
                                   else b

-- ejercicio 4; 2)

data TipoDePokemon = Agua | Fuego | Planta deriving Show
data Pokemon = ConsPokemon TipoDePokemon Int deriving Show
data Entrenador = ConsEntrenador String Pokemon Pokemon deriving Show

{-Dados dos Pokémon indica si el primero, en base al tipo, es superior al segundo. Agua
supera a fuego, fuego a planta y planta a agua. Y cualquier otro caso es falso.
-}
superaA :: Pokemon -> Pokemon -> Bool
superaA (ConsPokemon t1 e1) (ConsPokemon t2 e2) = superaATipo t1 t2

superaATipo :: TipoDePokemon -> TipoDePokemon -> Bool
superaATipo Agua Fuego = True
superaATipo Fuego Planta = True
superaATipo Planta Agua = True
superaATipo _ _ = False


pepo = ConsPokemon Agua 2
lola = ConsPokemon Agua 3
sami = ConsPokemon Planta 5

chalu = ConsPokemon Fuego 1
ash = ConsEntrenador "ash" pepo lola
juli = ConsEntrenador "juli" chalu sami

--Devuelve la cantidad de Pokémon de determinado tipo que posee el entrenador.
cantidadDePokemonDe :: TipoDePokemon -> Entrenador -> Int
cantidadDePokemonDe tp (ConsEntrenador nombre p1 p2) = contarTipo tp p1 + contarTipo tp p2


contarTipo :: TipoDePokemon -> Pokemon -> Int
contarTipo tipoBuscado (ConsPokemon tipo _) = unoSi (sonMismoTipo tipo tipoBuscado)

unoSi :: Bool -> Int
unoSi True = 1
unoSi _    = 0

sonMismoTipo :: TipoDePokemon -> TipoDePokemon -> Bool
sonMismoTipo Agua Agua = True
sonMismoTipo Fuego Fuego = True
sonMismoTipo Planta Planta = True
sonMismoTipo _ _ = False

--Dado un par de entrenadores, devuelve a sus Pokémon en una lista.
juntarPokemon :: (Entrenador, Entrenador) -> [Pokemon]
juntarPokemon (ConsEntrenador n p1 p2, ConsEntrenador nm pp1 pp2) = p1:p2:pp1:pp2:[]

-- ejercicio 5; 1)

--a
loMismo :: a -> a
loMismo a = a

--b
siempreSiete :: a -> Int
siempreSiete a = 7

--c
swap :: (a,b) -> (b, a)
swap (a,b) = (b, a)
--Existen dos variables de tipo diferente porque son una tupla.

-- ejercicio 5; 2)
--Son polifórmicas porque son versátiles y pueden trabajar con múltiples tipos de datos.

-- ejercicio 6;
--2
estaVacia :: [a] -> Bool
estaVacia [] = True
estaVacia _ = False

--3
--Precondición: La lista no puede estar vacia.
elPrimero :: [a] -> a
elPrimero (x : xs) = x

--4
--Precondición: La lista no puede estar vacía.
sinElPrimero :: [a] -> [a]
sinElPrimero (x : xs) = xs

--5
--Precondición: La lista no puede ser vacía.
splitHead :: [a] -> (a, [a])
splitHead [] = error "lista vacía"
splitHead (x : xs) = (x, xs)
