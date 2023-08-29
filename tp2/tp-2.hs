-- 1) Recursión sobre listas

-- 1.1) Dada una lista de enteros devuelve la suma de todos sus elementos.
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use foldr" #-}
{-# HLINT ignore "Use map" #-}

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
apariciones e (x:xs) = if e == x 
                        then 1 + apariciones e xs
                        else apariciones e xs

-- 1.9) Dados un número n y una lista xs, devuelve todos los elementos de xs que son menores a n.
losMenoresA :: Int -> [Int] -> [Int]
losMenoresA _ []     = []
losMenoresA n (x:xs) =  if n > x 
                            then x : losMenoresA n xs
                            else losMenoresA n xs

-- 1.10) Dados un número n y una lista de listas, devuelve la lista de aquellas listas que tienen más de n elementos.
lasDeLongitudMayorA :: Int -> [[a]] -> [[a]]
lasDeLongitudMayorA _ []     = []
lasDeLongitudMayorA n (x:xs) = if length x > n
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
zipMaximos [] []          = []
zipMaximos ns []         = ns
zipMaximos [] bs         = bs
zipMaximos (n:ns) (b:bs) = max n b : zipMaximos ns bs

-- 1.15) Dada una lista devuelve el mínimo.
--Precondición: La lista no puede ser vacía.
--elMinimo :: Ord a => [a] -> a
--elMinimo  [] = error "No se puede encontrar el mínimo en una lista vacía"
--elMinimo 

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
cuentaRegresiva n < 1    = []
cuentaRegresiva 1        = [1]
cuentaRegresiva n  =  n : cuentaRegresiva (n - 1)

