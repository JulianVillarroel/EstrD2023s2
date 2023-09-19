--  Ejercicios Integradores

-- 1. Pizzas
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use foldr" #-}
{-# HLINT ignore "Use map" #-}
{-# HLINT ignore "Use newtype instead of data" #-}
{-# HLINT ignore "Use record patterns" #-}
{-# HLINT ignore "Use infix" #-}

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
cantCapasPorPizza :: [Pizza] -> [(Int, Pizza)]
cantCapasPorPizza [] = []
cantCapasPorPizza (p:ps) = (cantIngredientes p, p) : cantCapasPorPizza ps

cantIngredientes :: Pizza -> Int
cantIngredientes Prepizza = 0
cantIngredientes (Capa i is) = 1 + cantIngredientes is


-- 2. Mapa de tesoros (con bifurcaciones)

data Dir    = Izq | Der                               deriving Show
data Objeto = Tesoro | Chatarra                       deriving Show
data Cofre  = Cofre [Objeto]                          deriving Show
data Mapa   = Fin Cofre | Bifurcacion Cofre Mapa Mapa deriving Show


--2.1.Indica si hay un tesoro en alguna parte del mapa. 
hayTesoro :: Mapa -> Bool
hayTesoro (Fin c) = tieneTesoro c
hayTesoro (Bifurcacion c iz de) = tieneTesoro c  || hayTesoro iz || hayTesoro de

tieneTesoro :: Cofre -> Bool
tieneTesoro (Cofre os) = contieneTesoro os

contieneTesoro :: [Objeto] -> Bool
contieneTesoro []     = False
contieneTesoro (o:os) = esTesoro o || contieneTesoro os

esTesoro :: Objeto -> Bool
esTesoro Tesoro = True
esTesoro _ = False


--2.1. Indica si al final del camino hay un tesoro. Nota: el final de un camino se representa con una
--lista vacía de direcciones.

hayTesoroEn :: [Dir] -> Mapa -> Bool
hayTesoroEn [] mp = tieneTesoro (primerCofre mp)
hayTesoroEn (d:ds) mp = hayBifurcacion mp && hayTesoroEn ds (ladoDeLaDireccion d mp)

hayBifurcacion :: Mapa -> Bool
hayBifurcacion (Bifurcacion _ _ _) = True
hayBifurcacion  _ = False

primerCofre :: Mapa -> Cofre
primerCofre (Fin c)             = c
primerCofre (Bifurcacion c _ _) = c

--Precondicion: Tiene que haber una bifurcacion.
ladoDeLaDireccion :: Dir -> Mapa -> Mapa
ladoDeLaDireccion _ (Fin _) = error "No hay mas mapa"
ladoDeLaDireccion d (Bifurcacion _ iz de) =
  if esDerecha d
    then de
    else iz

esDerecha :: Dir -> Bool
esDerecha Der = True
esDerecha _ = False

-- 2.3. Indica el camino al tesoro. Precondición: existe un tesoro y es único.
caminoAlTesoro :: Mapa -> [Dir]
caminoAlTesoro (Fin _) = []
caminoAlTesoro (Bifurcacion _ iz de) =
  if hayTesoro iz
    then Izq : caminoAlTesoro iz
    else Der : caminoAlTesoro de



mapaEjemplo =
    Bifurcacion (Cofre [Chatarra, Tesoro])
        (Bifurcacion (Cofre [Chatarra])
            (Fin (Cofre []))
            (Fin (Cofre [])))
        (Fin (Cofre []))


-- 2.4. Indica el camino de la rama más larga.
caminoDeLaRamaMasLarga :: Mapa -> [Dir]
caminoDeLaRamaMasLarga (Fin _) = []
caminoDeLaRamaMasLarga (Bifurcacion _ iz de) =
  if heightT iz < heightT de
    then Der : caminoDeLaRamaMasLarga de
    else Izq : caminoDeLaRamaMasLarga iz

heightT :: Mapa -> Int
heightT (Fin _)  = 0
heightT (Bifurcacion _ iz de) = 1 + max (heightT iz) (heightT de)


-- 2.5.Devuelve los tesoros separados por nivel en el árbol. 
tesorosPorNivel :: Mapa -> [[Objeto]]
tesorosPorNivel mapa = pasandoPorLevel (heightT mapa) mapa

pasandoPorLevel :: Int -> Mapa -> [[Objeto]]
pasandoPorLevel 0 mapa = [levelN 0 mapa]
pasandoPorLevel n mapa = levelN n mapa : pasandoPorLevel (n - 1) mapa

levelN :: Int -> Mapa -> [Objeto]
levelN _ (Fin c) = objetosDe c
levelN 0 (Bifurcacion c _ _) = objetosDe c
levelN n (Bifurcacion _ iz de) = levelN (n - 1) iz ++ levelN (n - 1) de

objetosDe :: Cofre -> [Objeto]
objetosDe (Cofre os) = os


-- 2.6. Devuelve todos lo caminos en el mapa
todosLosCaminos :: Mapa -> [[Dir]]
todosLosCaminos (Fin _) = []
todosLosCaminos (Bifurcacion _ iz de) = agregarDireccion Izq (todosLosCaminos iz) ++ agregarDireccion Der (todosLosCaminos de)

agregarDireccion :: Dir -> [[Dir]] -> [[Dir]]
agregarDireccion a []  = [[a]]
agregarDireccion a (l:ls) = (a : l) : agregarDireccion a ls


-- 3. Nave Espacia

data Componente = LanzaTorpedos | Motor Int | Almacen [Barril]  deriving Show
data Barril     = Comida | Oxigeno | Torpedo | Combustible      deriving Show

data Sector     = S SectorId [Componente] [Tripulante]          deriving Show
type SectorId   = String
type Tripulante = String

data Tree a     = EmptyT | NodeT a (Tree a) (Tree a)            deriving Show
data Nave       = N (Tree Sector)                               deriving Show



-- 3.1. Propósito: Devuelve todos los sectores de la nave
sectores :: Nave -> [SectorId]
sectores (N tree) = sectoresEnArbol tree

sectoresEnArbol :: Tree Sector -> [SectorId]
sectoresEnArbol EmptyT = []
sectoresEnArbol (NodeT sector iz de) = sectoresEnArbol iz ++ [sectorId sector] ++ sectoresEnArbol de

sectorId :: Sector -> SectorId
sectorId (S id _ _) = id


naveEjemplo =
  N (NodeT (S "s1" [] [])
    (NodeT (S "s2" [] []) EmptyT EmptyT)
    (NodeT (S "s3" [] []) EmptyT EmptyT))

naveConMotores =
  N (NodeT (S "s1" [Motor 100, Motor 200] [])
    (NodeT (S "s2" [Motor 150] []) EmptyT EmptyT)
    (NodeT (S "s3" [Almacen []] []) EmptyT EmptyT))


-- 3.2. Propósito: Devuelve la suma de poder de propulsión de todos los motores de la nave. Nota:
-- el poder de propulsión es el número que acompaña al constructor de motores

poderDePropulsion :: Nave -> Int
poderDePropulsion (N tree) = poderDePropulsionDeSectores tree

poderDePropulsionDeSectores :: Tree Sector -> Int
poderDePropulsionDeSectores EmptyT = 0
poderDePropulsionDeSectores (NodeT s iz de) = poderDePropulsionDeSector s + poderDePropulsionDeSectores iz + poderDePropulsionDeSectores de

poderDePropulsionDeComponente :: Componente -> Int
poderDePropulsionDeComponente (Motor poder) = poder
poderDePropulsionDeComponente _ = 0

poderDePropulsionDeComponentes :: [Componente] -> Int
poderDePropulsionDeComponentes [] = 0
poderDePropulsionDeComponentes (x:xs) = poderDePropulsionDeComponente x + poderDePropulsionDeComponentes xs

poderDePropulsionDeSector :: Sector -> Int
poderDePropulsionDeSector (S _ componentes _) = poderDePropulsionDeComponentes componentes

-- 3.Propósito: Devuelve todos los barriles de la nave.
barriles :: Nave -> [Barril]
barriles (N tree) = barrilesDe tree

barrilesDe :: Tree Sector -> [Barril]
barrilesDe EmptyT = []
barrilesDe (NodeT s iz de) = barrilesEnSector s ++ barrilesDe iz ++ barrilesDe de

barrilesEnSector :: Sector -> [Barril]
barrilesEnSector (S _ cs _) = barrilesEnComponentes cs

barrilesEnComponentes :: [Componente] -> [Barril]
barrilesEnComponentes [] = []
barrilesEnComponentes (c:cs) = barrilesEnComponente c ++ barrilesEnComponentes cs

barrilesEnComponente :: Componente -> [Barril]
barrilesEnComponente (Almacen bs) = bs
barrilesEnComponente _ = []


naveConBarriles =
  N (NodeT (S "Sector1" [Motor 100, Almacen [Comida, Oxigeno]] [])
    (NodeT (S "Sector2" [Motor 150, Almacen [Torpedo, Combustible]] []) EmptyT EmptyT)
    (NodeT (S "Sector3" [Almacen [Comida, Combustible]] []) EmptyT EmptyT))



-- 4. Propósito: Añade una lista de componentes a un sector de la nave.
-- Nota: ese sector puede no existir, en cuyo caso no añade componentes.
agregarASector :: [Componente] -> SectorId -> Nave -> Nave
agregarASector componente sector (N tree) = N (agregarASectorDelArbol componente sector tree)

agregarASectorDelArbol :: [Componente] -> SectorId -> Tree Sector -> Tree Sector
agregarASectorDelArbol _ _ EmptyT = EmptyT
agregarASectorDelArbol componentes id (NodeT a iz de) =
  NodeT (agrgarComponentesASi componentes a id) (agregarASectorDelArbol componentes id iz)  (agregarASectorDelArbol componentes id de)

agrgarComponentesASi :: [Componente] -> Sector -> SectorId -> Sector
agrgarComponentesASi cs (S s css ts) id =
  if s == id
    then  S s (css ++ cs) ts
    else S s css ts

-- 5.Propósito: Incorpora un tripulante a una lista de sectores de la nave.
-- Precondición: Todos los id de la lista existen en la nave.
--asignarTripulanteA :: Tripulante -> [SectorId] -> Nave -> Nave

-- 6.Propósito: Devuelve los sectores en donde aparece un tripulante dado.
-- sectoresAsignados :: Tripulante -> Nave -> [SectorId]

-- 7.Propósito: Devuelve la lista de tripulantes, sin elementos repetidos.
-- tripulantes :: Nave -> [Tripulante]

-- 4. Manada de lobos

type Presa = String                     -- nombre de presa
type Territorio = String                -- nombre de territorio
type Nombre = String                    -- nombre de lobo

data Lobo = Cazador Nombre [Presa] Lobo Lobo Lobo
          | Explorador Nombre [Territorio] Lobo Lobo
          | Cria Nombre                                  deriving Show
data Manada = M Lobo                                     deriving Show


--4.1.Construir un valor de tipo Manada que posea 1 cazador, 2 exploradores y que el resto sean crias

manadaEjemplo =
  M (Cazador "Cazador1" ["Conejo", "Ciervo"] 
    (Explorador "Explorador1" ["Bosque", "Río"] (Cria "Cría1") (Cria "Cría2")) 
    (Explorador "Explorador2" ["Montaña"] (Cria "Cría3") (Cria "Cría4")) (Cria "Cría5"))



-- 4.2.Propósito: dada una manada, indica si la cantidad de alimento cazado es mayor a la cantidad de crías.
buenaCaza :: Manada -> Bool
buenaCaza (M lobo) = cantidadDePresasCazadas lobo > cantidadDeCrias lobo

cantidadDeCrias :: Lobo -> Int
cantidadDeCrias (Cria _) = 1
cantidadDeCrias (Cazador _ _ l1 l2 l3) = cantidadDeCrias l1 + cantidadDeCrias l2 + cantidadDeCrias l3
cantidadDeCrias (Explorador _ _ l1 l2) = cantidadDeCrias l1 + cantidadDeCrias l2

cantidadDePresasCazadas :: Lobo -> Int
cantidadDePresasCazadas (Cria _) = 0
cantidadDePresasCazadas (Cazador _ presas l1 l2 l3) = length presas + cantidadDePresasCazadas l1 + cantidadDePresasCazadas l2 + cantidadDePresasCazadas l3
cantidadDePresasCazadas (Explorador _ _ l1 l2)      = cantidadDePresasCazadas l1 + cantidadDePresasCazadas l2

 
-- 3.Propósito: dada una manada, devuelve el nombre del lobo con más presas cazadas, junto
-- con su cantidad de presas. Nota: se considera que los exploradores y crías tienen cero presas
-- cazadas, y que podrían formar parte del resultado si es que no existen cazadores con más de
-- cero presas.
--elAlfa :: Manada -> (Nombre, Int)

-- 4.Propósito: dado un territorio y una manada, devuelve los nombres de los exploradores que
-- pasaron por dicho territorio.
losQueExploraron :: Territorio -> Manada -> [Nombre]
losQueExploraron territorio (M lobo) = exploradoresQuePasaronPorTerritorio territorio lobo

exploradoresQuePasaronPorTerritorio :: Territorio -> Lobo -> [Nombre]
exploradoresQuePasaronPorTerritorio _ (Cria _) = []
exploradoresQuePasaronPorTerritorio t (Explorador n ts l1 l2) =
  if elem t ts
    then n : exploradoresQuePasaronPorTerritorio t l1 ++ exploradoresQuePasaronPorTerritorio t l2
    else exploradoresQuePasaronPorTerritorio t l1 ++ exploradoresQuePasaronPorTerritorio t l2
exploradoresQuePasaronPorTerritorio t (Cazador _ _ l1 l2 l3) =
  exploradoresQuePasaronPorTerritorio t l1 ++ exploradoresQuePasaronPorTerritorio t l2 ++ exploradoresQuePasaronPorTerritorio t l3
 
-- 5.Propósito: dada una manada, denota la lista de los pares cuyo primer elemento es un territorio y cuyo segundo elemento es la lista de los nombres de los exploradores que exploraron
-- dicho territorio. Los territorios no deben repetirse.
--exploradoresPorTerritorio :: Manada -> [(Territorio, [Nombre])]
 
-- 6.Propósito: dado un nombre de cazador y una manada, indica el nombre de todos los
-- cazadores que tienen como subordinado al cazador dado (directa o indirectamente).
-- Precondición: hay un cazador con dicho nombre y es único.
superioresDelCazador :: Nombre -> Manada -> [Nombre]
superioresDelCazador n (M lobo) = subordinadosDelCazador n lobo

subordinadosDelCazador :: Nombre -> Lobo -> [Nombre]
subordinadosDelCazador _ (Cria _) = []
subordinadosDelCazador n (Explorador _ _ l1 l2) = subordinadosDelCazador n l1 ++ subordinadosDelCazador n l2
subordinadosDelCazador n (Cazador nombre _ l1 l2 l3) =
  if n == nombre
    then [nombre] ++ subordinadosDelCazador n l1 ++ subordinadosDelCazador n l2 ++ subordinadosDelCazador n l3
    else subordinadosDelCazador n l1 ++ subordinadosDelCazador n l2 ++ subordinadosDelCazador n l3
