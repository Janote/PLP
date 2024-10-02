-- Guia 1
{-

max2 :: (Float,Float) -> Float

normaVector :: (Float,Float) -> Float

c)

flip :: (a -> b -> c) -> b -> a -> c
(-) :: Num a => a -> a -> a
flip (-)
entonces...    a -> a -> a
flip (-)

d)
Al hacer aplicacion parcial... falta un parametro
tonces
predecesor :: a -> a

e)
evaluarEnCero = \f -> f 0

evaluarenCero :: (a ->b) - > b

Toma una funcion que va de a en b y devuelve b.

f)
dosVeces = \f -> f . f

dosVeces :: (a -> a) -> a -> a

g)

-}
-- 2)
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Eta reduce" #-}
{-# HLINT ignore "Redundant bracket" #-}
{-# HLINT ignore "Collapse lambdas" #-}
{-# HLINT ignore "Use map" #-}

curry' :: ((a, b) -> c) -> a -> b -> c
curry' f x y = f (x, y)

-- b
uncurry' :: (a -> b -> c) -> (a, b) -> c
uncurry' f (x, y) = f x y

-- c

-- sumar todos los elementos

sum' :: [Integer] -> Integer
sum' = sum

unir :: [a] -> [a] -> [a]
unir xs ys = foldr (:) ys xs

filter' :: (a -> Bool) -> [a] -> [a]
filter' p = foldr (\x acc -> if p x then x : acc else acc) []

map' :: (a -> b) -> [a] -> [b]
map' = map

mejorSegun :: (a -> a -> Bool) -> [a] -> a
mejorSegun p = foldr1 (\x acc -> if p x acc then x else acc)

-- 3
--- tiene sentido usar foldl porque el resultado se va acumulando en cada iteracion!!
sumasParciales :: (Num a) => [a] -> [a]
sumasParciales = foldl (\acc x -> if null acc then [x] else acc ++ [x + last acc]) []

sumaAlt :: [Integer] -> Integer
sumaAlt = foldr (-) 0

sumaAlt' :: [Integer] -> Integer
sumaAlt' = foldl (\acc x -> -acc + x) 0

-- 4

partes :: [a] -> [[a]]
partes = foldr (\x acc -> [x] : concatMap (\y -> [x : y]) acc) [[]]

{-

partes :: [a] -> [[a]]
partes = foldr (\x rs -> rs ++ map (x:) rs) [[]]

partes [1,2,3]

[[]] ++  map(1:) [2,3]
[[]] ++  map(1:) [[]] ++ map(2:) partes [3]
[[]] ++  map(1:) [[]] ++ map(2:) [[]] ++ map (3:) partes []
[[]] ++  map(1:) [[]] ++ map(2:) [[]] ++ map (3:) [[]]
[[]] ++  map(1:) [[]] ++ map(2:) [[]] ++ [[3]]
[[]] ++  map(1:) [[]] ++ map(2:) [[],[3]]
[[]] ++  map(1:) [[]] ++ [[2],[2,3]]
[[]] ++  map(1:) [[],[[2],[2,3]]

tenes que hacer un concat map con lo que ya tenes !!concat map ? x : acc
-}

prefijos :: [a] -> [[a]]
prefijos = foldl (\acc x -> acc ++ [last acc ++ [x]]) [[]]

sublistas :: [a] -> [[a]]
sublistas = foldl (\acc x -> acc ++ map (x :) acc) [[]]

-- foldr (\x acc -> acc ++ map (x :) acc) [[]]

-- Ejercicio 5: Elementos en posiciones pares no es estructural ya que al hacer tail xs estas haciendo la recursion sobre la estructura
{-
. La recursión de elementosEnPosicionesPares es global, ya que accede a un
resultado anterior: el de la recursión sobre la cola de la cola de la lista (es decir
tail xs)

-}

entrelazar :: [a] -> [a] -> [a]
entrelazar =
  foldr
    ( \x fr ys ->
        if null ys
          then x : fr []
          else x : head ys : fr (tail ys)
    )
    id

recr :: (a -> [a] -> b -> b) -> b -> [a] -> b
recr _ z [] = z
recr f z (x : xs) = f x xs (recr f z xs)

trim = recr (\x xs rec -> if x == ' ' then rec else x : xs) []

{-
insertarOrdenado :: (Ord a) => a -> [a] -> [a]
genLista :: a -> (a -> a) -> Integer -> [a]

-}

sacarUna :: (Eq a) => a -> [a] -> [a]
sacarUna k = recr (\x xs rec -> if x == k then xs else x : rec) []

-- porque no podes tomar decisiones de lo que tengas como lista en foldr , en cambio aca tenes como dos opciones siempre , xs y rec

insertarOrdenado :: (Ord a) => a -> [a] -> [a]
insertarOrdenado e = recr (\x xs rec -> if e < x then e : x : xs else x : rec) [e]

-- Ejercicio 7

-- primero quiero probar como es lo de las listas infinitas
esPrimo :: Integer -> Bool
esPrimo n
  | n <= 1 = False
  | otherwise = esPrimoAux (fromInteger (round (sqrt (fromInteger n)))) n

esPrimoAux :: Integer -> Integer -> Bool
esPrimoAux i n
  | i == 1 = True
  | n `mod` i == 0 = False
  | otherwise = esPrimoAux (i - 1) n

genLista :: a -> (a -> a) -> Int -> [a]
genLista m f 0 = []
genLista n f i = n : genLista (f n) f (i - 1)

desdeHasta :: Integer -> Integer -> [Integer]
desdeHasta desde hasta = [desde .. hasta]

mapPares :: (a -> b -> c) -> [(a, b)] -> [c]
mapPares f [] = []
mapPares f (x : xs) = uncurry f x : mapPares f xs

armarPares :: [a] -> [b] -> [(a, b)]
armarPares xs ys = (foldr (\x acc -> \ys -> (x, head ys) : acc (tail ys)) (const []) xs) ys

mapDoble :: (a -> b -> c) -> [a] -> [b] -> [c]
mapDoble f [] _ = []
mapDoble f (x : xs) (y : ys) = f x y : mapDoble f xs ys

sumaMat :: [[Int]] -> [[Int]] -> [[Int]]
sumaMat = zipWith (zipWith (+))

-- Ejercicio 10

foldNat :: (Integer -> Integer -> Integer) -> Integer -> Integer -> Integer
foldNat _ 0 z = z
foldNat f n z = f n (foldNat f (n - 1) z)

potencia :: Integer -> Integer -> Integer
potencia n y = foldNat (\_ acc -> n * acc) y 1

data Polinomio a
  = X
  | Cte a
  | Suma (Polinomio a) (Polinomio a)
  | Prod (Polinomio a) (Polinomio a)

evaluar :: (Num a) => a -> Polinomio a -> a
evaluar n (Cte a) = a
evaluar n X = n
evaluar n (Suma p1 p2) = evaluar n p1 + evaluar n p2
evaluar n (Prod p1 p2) = evaluar n p1 * evaluar n p2

{-
data AB a = Nil | Bin (AB a) a (AB a)

foldAB :: b -> (b -> a -> b -> b) -> AB a -> b
foldAB fNil fBin Nil = fNil
foldAB fNil fBin (Bin i v d) = fBin (rec i) v (rec d)
  where
    rec = foldAB fNil fBin

ramas :: AB a -> [[a]]
ramas = foldAB [] f
  where
    f i v d
      | null i && null d = [[v]]
      | null d = map (v :) i
      | null i = map (v :) d
      | otherwise = map (v :) i ++ map (v :) d

cantNodos :: AB a -> Integer
cantNodos = foldAB 0 (\i _ d -> 1 + i + d)

esNil :: AB a -> Bool
esNil arbol = case arbol of
  Nil -> True
  Bin i r d -> False

altura :: AB a -> Integer
altura = foldAB 0 (\i v d -> 1 + max i d)

-- recursion primitiva
raíz :: AB a -> a
raíz (Bin i v d) = v

recAB :: b -> (AB a -> a -> AB a -> b -> b -> b) -> AB a -> b
recAB z f x = case x of
  Nil -> z
  (Bin l v r) -> f l v r (rec l) (rec r)
  where
    rec = recAB z f

esABB :: (Ord a) => AB a -> Bool
esABB = recAB  True f
  where
    f l v r rl rr
      | esNil l && esNil r = True
      | esNil r = rl && raíz l <= v
      | esNil l = rr && v < raíz r
      | otherwise = rl && rr && raíz l <= v && v < raíz r

arbolito = Bin (Bin Nil 2 Nil) 1 (Bin Nil 3 Nil)

cantHojas :: AB a -> Integer
cantHojas = foldAB 0 f
  where
  f i v d
    |i == 0 && d == 0  = 1
    | otherwise  = i + d

espejo :: AB a -> AB a
espejo = foldAB Nil f
  where
  f i v d
    | esNil i && esNil d = Bin Nil v Nil
    | esNil i = Bin d v Nil
    | esNil d = Bin Nil v i
    | otherwise = Bin d v i
-- o foldab Nil (\i v d -> Bin d v i )

-- mismaEstructura :: AB a -> AB b -> Bool

-}

data AIH a = Hoja a | Bin (AIH a) (AIH a)

foldAIH :: (a -> b) -> (b -> b -> b) -> AIH a -> b
foldAIH fHoja _ (Hoja a) = fHoja a
foldAIH fHoja fBin (Bin i d) = fBin (rec i) (rec d)
  where
    rec = foldAIH fHoja fBin

tamaño :: AIH a -> Integer
tamaño = foldAIH (const 1) (+)

altura :: AIH a -> Integer
altura = foldAIH (const 1) max

-- Ejercicio 15

data RoseTree a = Rose a [RoseTree a] deriving (Eq)

foldRose :: (a -> [b] -> b) -> RoseTree a -> b
foldRose f (Rose n hijos) = f n (map (foldRose f) hijos)

(.) :: (b -> c) -> (a -> b) -> a -> c
(g . f) x = g (f x)

arbolitoconinformacion = Bin (Bin (Hoja 4) (Hoja 3)) (Hoja 10)

rosetre = Rose 4 [Rose 3 [], Rose 8 [], Rose 10 [Rose 9 []]]
