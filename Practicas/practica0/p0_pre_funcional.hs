{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use foldr" #-}
import GHC.Integer.GMP.Internals (sqrInteger)
import GHC.Float (sqrtDouble)
-- Guia 0.
{-
1)
a) 
null :: Foldable t => t a -> Bool

Con Foldable se refiere a estructuras de datos que se pueden doblar o reducir su valor, como por ejemplo listas, arboles, etc.

-t es la estructura , como por ejemplo la lista
-a es el tipo de la lista.

Comportamiento: Se encarga de decir si la estructura elegida es vacia (True)  o no(False).


b)

head:: [a] -> a

Comportamiento: Devuelve el primer elemento de la lista, si lo hay. En otro caso devuelve error.


c)

tail :: [a] -> [a]

Comportamiento: Devuelve la misma lista de entrada , a excepcion del primer elemento del argumento pasado.


d)

init :: [a] -> [a]

Comportamiento: Devuelve la lista sin su ultimo elemento.


e)

last :: [a] -> a

Comportamiento: Devuelve el ultimo elemento si lo hay.

f)

drop :: Int -> [a] -> [a]

Comportamiento: Saca de la lista desde el index 0 hasta el del Int mostrado
Ej: drop 1  [8,7,6] = [7,6]


g)

(++) :: [a] -> [a] -> [a]


Comportamiento: Une dos listas con el orden del parametro 1 al 2.


h)

Concat :: [a] -> [a] -> [a]

Comportamiento: Une la primer lista pasada al principio de la segunda.


Reverse :: [a] -> [a]


Comportamiento: Da vuelta el orden de los elementos de la lista.


elem :: (Foldable t, Eq a) => a -> t a -> Bool

Comportamiento: Indica si un elemento pertenece a una lista(T) o no(F).

-}


--Ej 2

--a
valorAbsoluto :: Float -> Float --que dado un número devuelve su valor absoluto.
valorAbsoluto n
               | n <= 0  = -n
               | otherwise = n

--b
bisiesto :: Int -> Bool
bisiesto a
          | a `mod`  400  == 0  = True
          | a `mod ` 100 == 0 = False
          | a `mod` 4 == 0 = True
          | otherwise = False

--c
factorial :: Int -> Int -- definida únicamente para enteros p ositivos, que computa el factorial.
factorial 0 = 0
factorial 1 = 1
factorial n = n * factorial (n-1)


--d
cantDivisoresPrimos :: Int -> Int --que dado un entero p ositivo devuelve la cantidad de divisores primos
cantDivisoresPrimos n = cantDivisoresPrimosAux n 2

cantDivisoresPrimosAux :: Int -> Int -> Int
cantDivisoresPrimosAux n i
                          | i > n = 0
                          | n `mod` i == 0   && esPrimo i = 1 + cantDivisoresPrimosAux (n `div` i ) i
                          | otherwise = cantDivisoresPrimosAux n (i+1)


-- Complejidad O(sqrt(n)) , fijate que si lo haces O(n) tarda un rato que es re notable

esPrimo :: Int -> Bool
esPrimo n | n <= 1 = False
          | otherwise = esPrimoAux n (floor (sqrt (fromIntegral n)))


esPrimoAux :: Int -> Int -> Bool
esPrimoAux n i
                | i == 1 = True
                | n `mod` i == 0 && i /= 1 = False
                | otherwise = esPrimoAux n (i-1)


--3
--3-a
inverso :: Float -> Maybe Float
inverso x | x == 0 = Nothing
          | otherwise = Just (1/x)

--3-b

aEntero :: Either Int Bool -> Int
aEntero result = case  result of
    Right bool -> if bool then 1 else 0
    Left number -> number



--4-a 
limpiar :: String -> String -> String
limpiar _ [] = "" -- o []
limpiar xs (y:ys)
                  | pertenece xs y = limpiar xs ys
                  | otherwise = y : limpiar xs ys


pertenece :: Eq a => [a] -> a -> Bool
pertenece  xs y
                | null xs = False
                | y == head xs = True
                | otherwise = pertenece (tail xs) y

-- Si usas la notacion x:xs usa pattern matching xq sino se rompe todo


esVacia :: [a] -> Bool
esVacia [] = True
esVacia _ = False

--4-b 
todosIguales :: [Int] -> Bool
todosIguales [] = True
todosIguales [x] = True
todosIguales (x:xs) | x == head xs = todosIguales xs
                    | otherwise = False



--4-c
difPromedio :: [Float] -> [Float]
difPromedio xs = difPromedioAux xs (sumarElementos xs / fromIntegral (length xs))

sumarElementos :: [Float] -> Float
sumarElementos [] = 0
sumarElementos (x:xs)  = x + sumarElementos xs



difPromedioAux :: [Float] -> Float -> [Float]
difPromedioAux [] _ = []
difPromedioAux (x:xs) promedio = (x - promedio) : difPromedioAux xs promedio


data AB a = Nil | Bin (AB a) a (AB a)


vacioAB :: AB a -> Bool
vacioAB Nil = True
vacioAB _  = False



negacionAB :: AB Bool -> AB Bool
negacionAB Nil  = Nil
negacionAB (Bin left value right) =  Bin (negacionAB left) (not value) (negacionAB right)




productoAb :: AB Int -> Int
productoAb Nil = 1 
productoAb (Bin left value right) =  value * productoAb(left) * productoAb(right)


miArbol :: AB Int
miArbol = Bin
    (Bin Nil 2 Nil)  -- Subárbol izquierdo
    3                -- Valor del nodo raíz
    (Bin Nil 4 Nil)  -- Subárbol derecho



