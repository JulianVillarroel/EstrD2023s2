-- 1. Cálculo de costos
-- Especificar el costo operacional de las siguientes funciones:
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use foldr" #-}
{-# HLINT ignore "Use map" #-}
{-# HLINT ignore "Use newtype instead of data" #-}

-- Constante,  --O(1)
-- Lineal,     --O(n)
-- Cuadrática, --O(n^2)


head' :: [a] -> a
head' (x:xs) = x --O(1)

sumar :: Int -> Int 
sumar x = x + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 --O(1)

factorial :: Int -> Int
factorial 0 = 1    
factorial n = n * factorial (n-1) --O(n)

longitud :: [a] -> Int
longitud []     = 0
longitud (x:xs) = 1 + longitud xs --O(n)

factoriales :: [Int] -> [Int]
factoriales []     = []
factoriales (x:xs) = factorial x : factoriales xs --O(n^2)

pertenece :: Eq a => a -> [a] -> Bool
pertenece n []     = False
pertenece n (x:xs) = n == x || pertenece n xs --O(n)

sinRepetidos :: Eq a => [a] -> [a]
sinRepetidos []     = []
sinRepetidos (x:xs) =
    if pertenece x xs 
        then sinRepetidos xs
        else x : sinRepetidos xs  --O(n^2)

-- equivalente a (++)
append :: [a] -> [a] -> [a]
append [] ys     = ys
append (x:xs) ys = x : append xs ys --O(n)

concatenar :: [String] -> String
concatenar []     = []
concatenar (x:xs) = x ++ concatenar xs --O(n)

takeN :: Int -> [a] -> [a]
takeN 0 xs = []
takeN n [] = []
takeN n (x:xs) = x : takeN (n-1) xs --O(n)

dropN :: Int -> [a] -> [a]
dropN 0 xs     = xs
dropN n []     = []
dropN n (x:xs) = dropN (n-1) xs --O(n)

partir :: Int -> [a] -> ([a], [a])
partir n xs = (takeN n xs, dropN n xs) --O(n)

minimo :: Ord a => [a] -> a
minimo [x]    = x
minimo (x:xs) = min x (minimo xs) --O(n)

sacar :: Eq a => a -> [a] -> [a]
sacar n []     = []
sacar n (x:xs) =
    if n == x
        then xs
        else x : sacar n xs --O(n)

ordenar :: Ord a => [a] -> [a]
ordenar [] = []
ordenar xs =
    let m = minimo xs
        in m : ordenar (sacar m xs) --O(n^2)


-- 2. Set (conjunto)
-- Un Set es un tipo abstracto de datos que consta de las siguientes operaciones:

data Set a = S [a] deriving Show
 
-- Crea un conjunto vacío.
emptyS :: Set a
emptyS = S[]

-- Dados un elemento y un conjunto, agrega el elemento al conjunto.
--addS :: Eq a => a -> Set a -> Set a
--addS 

-- Dados un elemento y un conjunto indica si el elemento pertenece al conjunto.
-- belongs :: Eq a => a -> Set a -> Bool

-- Devuelve la cantidad de elementos distintos de un conjunto.
-- sizeS :: Eq a => Set a -> Int

-- Borra un elemento del conjunto.
-- removeS :: Eq a => a -> Set a -> Set a

-- Dados dos conjuntos devuelve un conjunto con todos los elementos de ambos. conjuntos.
-- unionS :: Eq a => Set a -> Set a -> Set a

-- Dado un conjunto devuelve una lista con todos los elementos distintos del conjunto.
-- setToList :: Eq a => Set a -> [a]


-- 1. Implementar la variante del tipo abstracto Set con una lista que no tiene repetidos y guarda
-- la cantidad de elementos en la estructura.
-- Nota: la restricción Eq aparece en toda la interfaz se utilice o no en todas las operaciones
-- de esta implementación, pero para mantener una interfaz común entre distintas posibles
-- implementaciones estamos obligados a escribir así los tipos.
-- data Set a = Set [a] Int

-- 2. Como usuario del tipo abstracto Set implementar las siguientes funciones:

-- losQuePertenecen :: Eq a => [a] -> Set a -> [a]
-- Dados una lista y un conjunto, devuelve una lista con todos los elementos que pertenecen
-- al conjunto.
-- 
-- sinRepetidos :: Eq a => [a] -> [a]
-- Quita todos los elementos repetidos de la lista dada utilizando un conjunto como estructura auxiliar.
-- 
-- unirTodos :: Eq a => Tree (Set a) -> Set a
-- Dado un arbol de conjuntos devuelve un conjunto con la union de todos los conjuntos
-- del arbol.

-- 3. Implementar la variante del tipo abstracto Set que posee una lista y admite repetidos. En
-- otras palabras, al agregar no va a chequear que si el elemento ya se encuentra en la lista, pero
-- sí debe comportarse como Set ante el usuario (quitando los elementos repetidos al pedirlos,
-- por ejemplo). Contrastar la eficiencia obtenida en esta implementación con la anterior.
