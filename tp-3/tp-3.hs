-- 1. Tipos recursivos simples

-- 1.1) celdas con bolitas
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Eta reduce" #-}
{-# HLINT ignore "Use foldr" #-}
{-# HLINT ignore "Redundant if" #-}
{-# HLINT ignore "Use map" #-}

data Color = Azul | Rojo                       deriving Show
data Celda = Bolita Color Celda | CeldaVacia   deriving Show


-- Dados un color y una celda, indica la cantidad de bolitas de ese color. Nota: pensar si ya
-- existe una operación sobre listas que ayude a resolver el problema.
nroBolitas :: Color -> Celda -> Int
nroBolitas _ CeldaVacia    = 0
nroBolitas c (Bolita cr r) =
    if esMismoColor c cr
        then 1 + nroBolitas c r
        else nroBolitas c r


esMismoColor :: Color -> Color -> Bool
esMismoColor Azul Azul = True
esMismoColor Rojo Rojo = True
esMismoColor _ _ = False


celdaEjemplo = Bolita Azul (Bolita Rojo (Bolita Azul CeldaVacia))


-- Dado un color y una celda, agrega una bolita de dicho color a la celda.
poner :: Color -> Celda -> Celda
poner co c = Bolita co c

-- Dado un color y una celda, quita una bolita de dicho color de la celda. Nota: a diferencia de
-- Gobstones, esta función es total.
sacar :: Color -> Celda -> Celda
sacar _ CeldaVacia    = CeldaVacia
sacar c (Bolita co r) =
    if sonIguales c co
        then r
        else Bolita co (sacar co r)


sonIguales :: Color -> Color -> Bool
sonIguales Rojo Rojo = True
sonIguales Azul Azul = True
sonIguales _ _       = False

-- Dado un número n, un color c, y una celda, agrega n bolitas de color c a la celda
ponerN :: Int -> Color -> Celda -> Celda
ponerN 0 _ c  = c
ponerN n c ce = ponerN (n - 1) c (poner c ce)

-- 1.2) Camino hacia el tesoro

data Objeto = Cacharro | Tesoro                          deriving Show
data Camino = Fin | Cofre [Objeto] Camino | Nada Camino  deriving Show

-- Indica si hay un cofre con un tesoro en el camino.
hayTesoro :: Camino -> Bool
hayTesoro Fin           = False
hayTesoro (Cofre obs c) = contieneTesoro obs || hayTesoro c
hayTesoro (Nada c)      = hayTesoro c

contieneTesoro :: [Objeto] -> Bool
contieneTesoro []     = False
contieneTesoro (o:os) = esTesoro o || contieneTesoro os


esTesoro :: Objeto -> Bool
esTesoro Tesoro = True
esTesoro _ = False


caminoConTesoro         = Cofre [Cacharro, Tesoro] (Nada (Cofre [Tesoro, Cacharro] Fin))
caminoConTesoroMuyLejos = Nada (Nada (Nada (Nada (Nada (Nada (Nada (Nada (Nada (Nada (Cofre [Cacharro] Fin)))))))))) -- 10 pasos hasta el cofre
caminoSinTesoro         = Nada Fin
caminoConVariosTesoros  = Cofre [Tesoro, Tesoro] Fin

{-Indica la cantidad de pasos que hay que recorrer hasta llegar al primer cofre con un tesoro.
Si un cofre con un tesoro está al principio del camino, la cantidad de pasos a recorrer es 0.
Precondición: tiene que haber al menos un tesoro.
-}
pasosHastaTesoro :: Camino -> Int
pasosHastaTesoro Fin                     = error "No hay tesoro"
pasosHastaTesoro (Cofre objetos camino)  =
  if contieneTesoro objetos
    then 0
    else 1 + pasosHastaTesoro camino
pasosHastaTesoro (Nada resto)            = 1 + pasosHastaTesoro resto

-- Indica si hay un tesoro en una cierta cantidad exacta de pasos. Por ejemplo, si el número de
-- pasos es 5, indica si hay un tesoro en 5 pasos.
hayTesoroEn :: Int -> Camino -> Bool
hayTesoroEn 0 (Cofre obs _)  = contieneTesoro obs
hayTesoroEn n (Cofre _ c) = hayTesoroEn (n - 1) c
hayTesoroEn n (Nada c) = hayTesoroEn (n - 1) c
hayTesoroEn _ _        = False

-- Indica si hay al menos n tesoros en el camino.
alMenosNTesoros :: Int -> Camino -> Bool
alMenosNTesoros 0 _             = True
alMenosNTesoros n Fin           = False
alMenosNTesoros n (Cofre obs c) =
  if n <= contarTesorosEnLista obs
    then True 
    else  alMenosNTesoros (n - contarTesorosEnLista obs) c
alMenosNTesoros n (Nada c)      = alMenosNTesoros n c

contarTesorosEnLista :: [Objeto] -> Int
contarTesorosEnLista []             = 0
contarTesorosEnLista (Tesoro : obs) = 1 + contarTesorosEnLista obs
contarTesorosEnLista (_ : obs)      = contarTesorosEnLista obs

-- 2. Tipos arbóreos

data Tree a = EmptyT | NodeT a (Tree a) (Tree a)  deriving Show

-- 1. Dado un árbol binario de enteros devuelve la suma entre sus elementos.
sumarT :: Tree Int -> Int
sumarT EmptyT          = 0
sumarT (NodeT a ai ad) = a + sumarT ai + sumarT ad

-- 2. Dado un árbol binario devuelve su cantidad de elementos, es decir, el tamaño del árbol (size en inglés).
sizeT :: Tree a -> Int
sizeT EmptyT          = 0
sizeT (NodeT _ iz de) = 1 + sizeT iz + sizeT de

-- 3. Dado un árbol de enteros devuelve un árbol con el doble de cada número.
mapDobleT :: Tree Int -> Tree Int
mapDobleT EmptyT          = EmptyT
mapDobleT (NodeT a iz de) = NodeT (a * 2) (mapDobleT iz) (mapDobleT de)


ejemploDeArbol :: Tree Int
ejemploDeArbol = NodeT 10
                    (NodeT 6
                        EmptyT
                        EmptyT)
                    (NodeT 16
                        (NodeT 14
                          EmptyT
                          EmptyT)
                        EmptyT)


ejemploDeArbolSoloConRaiz :: Tree Int
ejemploDeArbolSoloConRaiz = NodeT 10
                               EmptyT  EmptyT


-- 4. Dados un elemento y un árbol binario devuelve True si existe un elemento igual a ese en el árbol
perteneceT :: Eq a => a -> Tree a -> Bool
perteneceT _ EmptyT          = False
perteneceT a (NodeT b iz de) = b == a  || perteneceT a iz || perteneceT a de

-- 5. Dados un elemento e y un árbol binario devuelve la cantidad de elementos del árbol que son iguales a e.
aparicionesT :: Eq a => a -> Tree a -> Int
aparicionesT _ EmptyT          = 0
aparicionesT a (NodeT b iz de) =
  (if b == a
     then 1
       else 0) + aparicionesT a iz + aparicionesT a de

-- 6. Dado un árbol devuelve los elementos que se encuentran en sus hojas.
leaves :: Tree a -> [a]
leaves EmptyT                  = []
leaves (NodeT a EmptyT EmptyT) = [a]
leaves (NodeT _ iz de) = leaves iz ++ leaves de

{-7. Dado un árbol devuelve su altura.
Nota: la altura de un árbol (height en inglés), también llamada profundidad, es la cantidad
de niveles del árbol1
. La altura para EmptyT es 0, y para una hoja es 1.
-}
heightT :: Tree a -> Int
heightT EmptyT          = 0
heightT (NodeT _ iz de) = 1 + max (heightT iz) (heightT de)

-- 8. Dado un árbol devuelve el árbol resultante de intercambiar el hijo izquierdo con el derecho,
-- en cada nodo del árbol.
mirrorT :: Tree a -> Tree a
mirrorT EmptyT          = EmptyT
mirrorT (NodeT a iz de) = NodeT a (mirrorT de) (mirrorT iz)

{-9. Dado un árbol devuelve una lista que representa el resultado de recorrerlo en modo in-order.
Nota: En el modo in-order primero se procesan los elementos del hijo izquierdo, luego la raiz
y luego los elementos del hijo derecho.
-}
toList :: Tree a -> [a]
toList EmptyT          = []
toList (NodeT a iz de) = toList iz ++ [a] ++ toList de

{-10. Dados un número n y un árbol devuelve una lista con los nodos de nivel n. El nivel de un
nodo es la distancia que hay de la raíz hasta él. La distancia de la raiz a sí misma es 0, y la
distancia de la raiz a uno de sus hijos es 1.
Nota: El primer nivel de un árbol (su raíz) es 0.
-}
levelN :: Int -> Tree a -> [a]
levelN _ EmptyT = []
levelN 0 (NodeT b _ _) = [b]
levelN n (NodeT _ iz de) = levelN (n - 1) iz ++ levelN (n - 1) de

-- 11. Dado un árbol devuelve una lista de listas en la que cada elemento representa un nivel de dicho árbol
listPerLevel :: Tree a -> [[a]]
listPerLevel tree = pasandoPerLevel (heightT tree) tree

pasandoPerLevel :: Int -> Tree a -> [[a]]
pasandoPerLevel 0 tree = [levelN 0 tree]
pasandoPerLevel n tree = levelN n tree : pasandoPerLevel (n - 1) tree

-- 12. Devuelve los elementos de la rama más larga del árbol
ramaMasLarga :: Tree a -> [a]
ramaMasLarga EmptyT          = []
ramaMasLarga (NodeT a iz de) =
  if heightT iz < heightT de
    then a : ramaMasLarga de
    else a : ramaMasLarga iz



-- 13. Dado un árbol devuelve todos los caminos, es decir, los caminos desde la raíz hasta cualquiera de los nodos.
todosLosCaminos :: Tree a -> [[a]]
todosLosCaminos EmptyT = []
todosLosCaminos (NodeT a iz de) = agregarElemento a (todosLosCaminos iz ++ todosLosCaminos de)

agregarElemento :: a -> [[a]] -> [[a]]
agregarElemento a []  = [[a]]
agregarElemento a (l:ls) = (a : l) : agregarElemento a ls 

-- 2.2. Expresiones Aritméticas

data ExpA = Valor Int
            | Sum ExpA ExpA
            | Prod ExpA ExpA
            | Neg ExpA  deriving Show


-- 1. Dada una expresión aritmética devuelve el resultado evaluarla
eval :: ExpA -> Int
eval (Valor n)    = n
eval (Sum e1 e2)  = eval e1 + eval e2
eval (Prod e1 e2) = eval e1 * eval e2
eval (Neg e)      = -eval e

{-2. Dada una expresión aritmética, la simplifica según los siguientes criterios (descritos utilizando
notación matemática convencional):
          a) 0 + x = x + 0 = x
          b) 0 * x = x * 0 = 0
          c) 1 * x = x * 1 = x
          d) - (- x) = x
-}
simplificar :: ExpA -> ExpA
simplificar (Sum (Valor 0) e)  = simplificar e                          -- a) 0 + x = x
simplificar (Sum e (Valor 0))  = simplificar e                          -- a) x + 0 = x
simplificar (Prod (Valor 0) _) = Valor 0                                -- b) 0 * x = 0
simplificar (Prod _ (Valor 0)) = Valor 0                                -- b) x * 0 = 0
simplificar (Prod (Valor 1) e) = simplificar e                          -- c) 1 * x = x
simplificar (Prod e (Valor 1)) = simplificar e                          -- c) x * 1 = x
simplificar (Neg (Neg e))      = simplificar e                          -- d) -(-x) = x
simplificar (Sum e1 e2)        = Sum (simplificar e1) (simplificar e2)
simplificar (Prod e1 e2)       = Prod (simplificar e1) (simplificar e2)
simplificar (Neg e)            = Neg (simplificar e)
simplificar e                  = e                                      -- cualquier otro caso queda igual



ejemplo :: ExpA
ejemplo = Sum (Sum (Valor 0) (Prod (Valor 1) (Valor 3))) (Sum (Valor 0) (Neg (Neg (Valor 5))))