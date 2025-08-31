--Ejercicio 1

-- no currificada recibe una tupla
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use sum" #-}
max2 :: (Float, Float) -> Float
max2 (x,y)
    | x >= y = x
    | otherwise = y

-- version currificada recibe los parametros de a uno
max2' :: Float -> Float -> Float
max2' x y 
    | x >= y = x
    | otherwise = y

normaVectorial :: (Float, Float) -> Float
normaVectorial (x, y) = sqrt(x^2 + y^2)

normaVectorial' :: Float -> Float -> Float
normaVectorial' x y = sqrt(x^2 + y^2)

-- esta funcion ya viene currificada y todas las demas de abajo
subtract' :: Float -> Float -> Float
subtract' = flip (-)

predecesor :: Float -> Float
predecesor = subtract' 1 

evaluarEnCero :: (Float -> Float) -> Float
evaluarEnCero = \f -> f 0

dosVeces :: (Float -> Float) -> (Float -> Float) 
dosVeces = \f -> f . f

flipAll :: [Float -> Float -> Float] -> [Float -> Float -> Float]
flipAll = map flip

flipRaro :: Float -> (Float -> Float -> Float) -> Float -> Float
flipRaro = flip flip

--- Fin ejercico 1


 -- Ejercicio 2
curry :: ((a,b) -> c) -> a -> b -> c
curry f x y = f (x,y)

uncurry:: (a -> b -> c) -> (a, b) -> c
uncurry f (x,y) = f x y

-- curryN no se puede para todo N hay que implementar ciertos trucos
{-
En Haskell puro, no se puede definir una función curryN completamente genérica 
que tome una función de cualquier cantidad de argumentos y la currifique, 
porque el sistema de tipos de Haskell no permite manipular funciones de aridad arbitraria 
de forma genérica (sin usar técnicas avanzadas).
-}

-- Ejercicio 3
sumatoria ::Num a => [a] -> a
sumatoria = foldr (+) 0 

elem' :: Eq a => a -> [a] -> Bool
elem' x = foldr (\y acc -> y == x || acc) False

concatListas :: [a] -> [a] -> [a]
concatListas xs ys = foldr (:) ys xs