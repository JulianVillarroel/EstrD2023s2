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

ejemplo3 = sucesor (maxDelPar(divisionYResto (sumar 2 7) 10))

ejemplo4 = sumar (maxDelPar (divisionYResto 10 2)) (sucesor 4)