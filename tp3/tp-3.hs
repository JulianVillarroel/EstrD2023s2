-- 1. Tipos recursivos simples

-- 1.1) celdas con bolitas

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
poner c CeldaVacia    = Bolita c CeldaVacia
poner c (Bolita co r) = Bolita co (poner c r)

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
sonIguales _ _ = False

-- Dado un número n, un color c, y una celda, agrega n bolitas de color c a la celda
ponerN :: Int -> Color -> Celda -> Celda
ponerN 0 _ c = c
ponerN n c ce = ponerN (n - 1) c (poner c ce)

-- 1.2) Camino hacia el tesoro

data Objeto = Cacharro | Tesoro                          deriving Show
data Camino = Fin | Cofre [Objeto] Camino | Nada Camino  deriving Show

-- Indica si hay un cofre con un tesoro en el camino.
hayTesoro :: Camino -> Bool
hayTesoro Fin = False
hayTesoro (Cofre obs c) = contieneTesoro obs || hayTesoro c 
hayTesoro (Nada c) = hayTesoro c

contieneTesoro :: [Objeto] -> Bool
contieneTesoro []       = False
contieneTesoro (t : _)  = True 
contieneTesoro (_ : os) = contieneTesoro os


caminoConTesoro         = Cofre [Cacharro, Tesoro] (Nada (Cofre [Tesoro, Cacharro] Fin))
caminoConTesoroMuyLejos = Nada (Nada (Nada (Nada (Nada (Nada (Nada (Nada (Nada (Nada (Cofre [Cacharro] Fin)))))))))) -- 10 pasos hasta el cofre
caminoSinTesoro         = Nada (Fin)


{-Indica la cantidad de pasos que hay que recorrer hasta llegar al primer cofre con un tesoro.
Si un cofre con un tesoro está al principio del camino, la cantidad de pasos a recorrer es 0.
Precondición: tiene que haber al menos un tesoro.
-}

pasosHastaTesoro :: Camino -> Int
pasosHastaTesoro Fin = error "No hay tesoro"
pasosHastaTesoro (Cofre _ _) = 0
pasosHastaTesoro (Nada resto) = 1 + pasosHastaTesoro resto


-- Indica si hay un tesoro en una cierta cantidad exacta de pasos. Por ejemplo, si el número de
-- pasos es 5, indica si hay un tesoro en 5 pasos.
hayTesoroEn :: Int -> Camino -> Bool
hayTesoroEn n camino = hayTesoroEnAux n camino 0

hayTesoroEnAux :: Int -> Camino -> Int -> Bool
hayTesoroEnAux 0 (Cofre obs _) _ = contieneTesoro obs
hayTesoroEnAux n (Cofre _ c) pasos = hayTesoroEnAux (n - 1) c (pasos + 1)
hayTesoroEnAux n (Nada c) pasos = hayTesoroEnAux n c (pasos + 1)
hayTesoroEnAux _ Fin _ = False

-- Indica si hay al menos n tesoros en el camino.
alMenosNTesoros :: Int -> Camino -> Bool
alMenosNTesoros n camino = alMenosNTesorosEnElCamino n camino

alMenosNTesorosEnElCamino :: Int -> Camino -> Bool
alMenosNTesorosEnElCamino 0 _ = True
alMenosNTesorosEnElCamino n Fin = False
alMenosNTesorosEnElCamino n (Cofre obs c) =
  alMenosNTesorosEnElCamino (n - contarTesorosEnLista obs) c
alMenosNTesorosEnElCamino n (Nada c) = alMenosNTesorosEnElCamino n c

contarTesorosEnLista :: [Objeto] -> Int
contarTesorosEnLista [] = 0
contarTesorosEnLista (Tesoro : obs) = 1 + contarTesorosEnLista obs
contarTesorosEnLista (_ : obs) = contarTesorosEnLista obs

-- 2. Tipos arbóreos

data Tree a = EmptyT | NodeT a (Tree a) (Tree a)  deriving Show

-- 1. Dado un árbol binario de enteros devuelve la suma entre sus elementos.
sumarT :: Tree Int -> Int
sumarT EmptyT          = 0
sumarT (NodeT a ai ad) = a + sumarT ai + sumarT ad

-- 2. Dado un árbol binario devuelve su cantidad de elementos, es decir, el tamaño del árbol (size en inglés).
sizeT :: Tree a -> Int
sizeT EmptyT = 0
sizeT (NodeT _ iz de) = 1 + sizeT iz + sizeT de