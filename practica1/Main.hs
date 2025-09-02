{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use sum" #-}
{-# HLINT ignore "Redundant lambda" #-}
{-# HLINT ignore "Use map" #-}
{-# HLINT ignore "Use const" #-}

--Ejercicio 1

-- no currificada recibe una tupla
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
normaVectorial (x, y) = sqrt (x^2 + y^2)

normaVectorial' :: Float -> Float -> Float
normaVectorial' x y = sqrt (x^2 + y^2)

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

-- ejmplo mio de como usar foldr
todosIguales :: Eq a => [a] -> Bool
todosIguales [] = True
todosIguales (x : xs) = foldr (\y acc -> y == x && acc) True xs

filter' :: (a -> Bool) -> [a] -> [a]
filter' p = foldr (\x acc -> if p x then x : acc else acc) []

map' :: (a -> b) -> [a] -> [b]
map' p = foldr (\x acc -> p x : acc) []

mejorSegun :: (a -> a -> Bool) -> [a] -> a
mejorSegun p = foldr1 (\x y -> if p x y then x else y)

sumasParciales :: Num a => [a] -> [a]
sumasParciales xs = reverse (snd (foldl f (0, []) xs))
    where 
        f (suma, acc) x =
            let nuevaSuma = suma + x
            in (nuevaSuma, nuevaSuma : acc)


-- Variante usando foldr (técnica de continuaciones) sin reverse adicional
-- Idea: foldr construye una función que, dado el acumulado previo (suma de elementos a la izquierda), produce la lista de sumas parciales restantes.
-- step x k accPrev = let s = accPrev + x in s : k s
sumasParcialesR :: (Num a) => [a] -> [a]
sumasParcialesR xs = foldr step (\_ -> []) xs 0
  where
    -- step toma el elemento actual x y la "continuación" k (que dado un acumulado previo
    -- produce el resto de las sumas parciales). Devuelve una nueva función que espera
    -- el acumulado previo de la parte izquierda.
    -- Derivación: si antes llevábamos accPrev, la primera suma nueva es s = accPrev + x
    -- y luego seguimos con k s.
    step x k accPrev = let s = accPrev + x in s : k s

{-
Derivación resumida de sumasParcialesR:

Objetivo: dadas [x1,x2,...,xn] producir [x1, x1+x2, ..., x1+...+xn].

foldr procesa de derecha a izquierda, así que al estar en xi necesitamos saber
"qué hacer" cuando más tarde conozcamos la suma previa de los elementos a su izquierda.
Por eso acumulamos una función k :: a -> [a] (continuación dependiente de la suma previa).

Base: para [] no hay parciales -> k0 = \_ -> []
Paso: dado x y k (para la cola), definimos
  step x k accPrev = let s = accPrev + x in s : k s
Finalmente aplicamos la función obtenida a 0 (suma previa inicial).
Esto evita construir la lista al revés y luego hacer reverse.
-}

