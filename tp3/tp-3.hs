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


