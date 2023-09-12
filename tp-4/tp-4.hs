--  Ejercicios Integradores

-- 1. Pizzas
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use foldr" #-}

data Pizza = Prepizza
    | Capa Ingrediente Pizza       deriving Show
data Ingrediente = Salsa
    | Queso
    | Jamon
    | Aceitunas Int                deriving Show

-- Dada una pizza devuelve la cantidad de ingredientes
cantidadDeCapas :: Pizza -> Int
cantidadDeCapas Prepizza = 0
cantidadDeCapas (Capa _ p) = 1 + cantidadDeCapas p



pizzaEj =
  Capa Salsa (
    Capa Queso (
      Capa Jamon (
        Capa (Aceitunas 8)
          Prepizza )))


-- Dada una lista de ingredientes construye una pizza
armarPizza :: [Ingrediente] -> Pizza
armarPizza [] = Prepizza
armarPizza (i:is) = Capa i (armarPizza is)

-- Le saca los ingredientes que sean jamón a la pizza
sacarJamon :: Pizza -> Pizza
sacarJamon Prepizza = Prepizza
sacarJamon (Capa Jamon p) = sacarJamon p
sacarJamon (Capa o p)     = Capa o (sacarJamon p)

-- Dice si una pizza tiene solamente salsa y queso (o sea, no tiene de otros ingredientes. En
--particular, la prepizza, al no tener ningún ingrediente, debería dar verdadero.)
tieneSoloSalsaYQueso :: Pizza -> Bool
tieneSoloSalsaYQueso Prepizza    = True
tieneSoloSalsaYQueso (Capa i is) = esSalsaOQueso i && tieneSoloSalsaYQueso is

esSalsaOQueso :: Ingrediente -> Bool
esSalsaOQueso Salsa = True
esSalsaOQueso Queso = True
esSalsaOQueso _ = False

pizzaSYQ =
  Capa Salsa (
    Capa Queso 
        Prepizza)

-- Recorre cada ingrediente y si es aceitunas duplica su cantidad
duplicarAceitunas :: Pizza -> Pizza
duplicarAceitunas Prepizza    = Prepizza
duplicarAceitunas (Capa i is) = Capa (duplicarAceituna i) (duplicarAceitunas is) 

duplicarAceituna :: Ingrediente -> Ingrediente
duplicarAceituna (Aceitunas n) = Aceitunas (n * 2)
duplicarAceituna i = i

pizzaDeAceituna =
    Capa Salsa (
        Capa Queso (
            Capa (Aceitunas 2) 
                Prepizza))
                                           
                                           

-- Dada una lista de pizzas devuelve un par donde la primera componente es la cantidad de
-- ingredientes de la pizza, y la respectiva pizza como segunda componente.
-- cantCapasPorPizza :: [Pizza] -> [(Int, Pizza)]
-- cantCapasPorPizza [] = []
-- cantCapasPorPizza (p:ps) = cantCapasPorCadaPizza p : (cantCapasPorPizza ps)
--  
-- 
-- 
-- cantCapasPorCadaPizza :: Pizza -> (Int,Pizza)
-- cantCapasPorCadaPizza (Capa i is) = unoSi (esIngrediente i) : (cantCapasPorPizza is)
-- 
-- unoSi :: Bool -> Int 
-- unoSi True  = 1
-- unoSi False = 0
-- 
-- esIngrediente :: Ingrediente -> Bool
-- esIngrediente Queso         = True
-- esIngrediente Jamon         = True
-- esIngrediente (Aceitunas _) = True
-- esIngrediente _             = False


