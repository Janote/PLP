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
{-# HLINT ignore "Use sum" #-}

curry :: ((a, b) -> c) -> a -> b -> c
curry f x y = f (x, y)

uncurry :: (a -> b -> c) -> (a, b) -> c
uncurry f (x, y) = f x y

{- Me parece que no porque hay infinitos tipos de datos y cuesta generalizarlo-}

-- 3)

sum' :: (Num a) => [a] -> a
sum' = foldr (+) 0

unirListas :: [a] -> [a] -> [a]
unirListas xs ys = foldr (:) ys xs


filter' :: (a -> Bool) -> [a] -> [a]
filter' p = foldr ( \x rec -> if p x then x: rec  else rec ) [] 


map' :: (a -> b) -> [a] -> [b]
map' f xs = foldr(\x rec ->  f x : rec ) [] xs  
-- nota : se puede referir con rec , a los proximos pasos recursivos , como vimos en filter y map....

mejorSegun ::  Num a => (a -> a -> Bool) -> [a] -> a
mejorSegun p = foldr( \x rec -> if p x rec then x else rec) 0

sumaAlt :: [Integer] -> Integer
sumaAlt = foldr(\x rec -> x - rec) 0  
-- resta de todos -x + rec



