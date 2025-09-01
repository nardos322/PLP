import Language.Haskell.TH (safe)

absoluto :: Int -> Int
absoluto x
  | x > 0 = x
  | otherwise = -x

factorial :: Int -> Int
factorial 0 = 1
factorial 1 = 1
factorial n = n * factorial (n - 1)

-- //// version cantDivisoresPrimos que se me ocurrio /////////////////////

menorDivisorDesde :: Integer -> Integer -> Integer
menorDivisorDesde n k
  | mod n k == 0 = k
  | n == k = n
  | otherwise = menorDivisorDesde n (k + 1)

menorDivisor :: Integer -> Integer
menorDivisor n = menorDivisorDesde n 2

esPrimo :: Integer -> Bool
esPrimo n
  | n < 2 = False
  | otherwise = n == menorDivisor n

cantDivisoresPrimos :: Integer -> Integer
cantDivisoresPrimos n = cantDivisoresPrimosAux n 2

cantDivisoresPrimosAux :: Integer -> Integer -> Integer
cantDivisoresPrimosAux n k
  | n == k && not (esPrimo n) = 0
  | n == k && esPrimo n = 1
  | esPrimo k && mod n k == 0 = 1 + cantDivisoresPrimosAux n (k + 1)
  | otherwise = cantDivisoresPrimosAux n (k + 1)

-- //////////////////////////////////////////////////////////////////////////////////////

-- otra version de cantDivsoresPrimos mejor que aprovecha ventajas de haskell

-- Devuelve True si el numero es primo
esPrimo2 :: Integer -> Bool
esPrimo2 n = n > 1 && all (\k -> n `mod` k /= 0) [2 .. (floor . sqrt . fromIntegral) n]

-- Devuelve la cantidad de divisores primos de n
cantDivisoresPrimos2 :: Integer -> Integer
cantDivisoresPrimos2 n = fromIntegral . length $ filter (\k -> n `mod` k == 0 && esPrimo2 k) [2 .. n]

-- Ejercicios para entender que hace Maybe y como se usa, sirve para hacer una funcion segura
safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x : _) = Just x

-- Division segura
safeDivide :: Double -> Double -> Maybe Double
safeDivide _ 0 = Nothing
safeDivide x y = Just (x / y)

divide :: Double -> Double -> Double
divide x y = x / y

{- Either
Para que sirve?
\* Manejo de errores con informacion: Left para errores, Right para exito
\* Valores de dos tipos posibles: Cuando una funcion puede retornar dos tipos diferentes
\* Valores opcionales: Mantener informacion sobre que salio mal -}

-- Ejemplos de uso

-- Division con mensaje de error
divideWithError :: Double -> Double -> Either String Double
divideWithError _ 0 = Left "Error: Division por cero"
divideWithError x y = Right (x / y)

-- Parear un numero
parseNumber :: String -> Either String Int
parseNumber str = case reads str of
  [(n, "")] -> Right n
  _ -> Left ("no se pudo parsear: " ++ str)

{- EJEMPLOS DE FUNCIONES QUE RECIBEN Either Y DEVUELVEN Int -}

-- Ejemplo 1: Either Int Bool -> Int
-- Si es Left (Int), devuelve ese número
-- Si es Right (Bool), devuelve 1 si True, 0 si False
convertirAInt :: Either Int Bool -> Int
convertirAInt (Left numero) = numero
convertirAInt (Right True) = 1
convertirAInt (Right False) = 0

-- Ejemplo 2: Either String Int -> Int
-- Si es Left (error), devuelve -1
-- Si es Right (número), devuelve ese número
extraerNumero :: Either String Int -> Int
extraerNumero (Left _) = -1 -- Cualquier error = -1
extraerNumero (Right n) = n

-- Ejemplo 3: Either Char Int -> Int
-- Si es Left (caracter), devuelve su código ASCII
-- Si es Right (número), devuelve ese número
charONumero :: Either Char Int -> Int
charONumero (Left c) = fromEnum c -- Código ASCII del char
charONumero (Right n) = n

-- Ejemplo 4: Either [Int] Int -> Int
-- Si es Left (lista), devuelve la suma de la lista
-- Si es Right (número), devuelve ese número
sumaONumero :: Either [Int] Int -> Int
sumaONumero (Left lista) = sum lista
sumaONumero (Right n) = n

-- Ejemplo 5: Either Double Int -> Int
-- Si es Left (Double), devuelve la parte entera
-- Si es Right (Int), devuelve ese número
redondearODevolver :: Either Double Int -> Int
redondearODevolver (Left d) = round d
redondearODevolver (Right n) = n

{- EJEMPLOS DE USO -}
-- Llamadas a las funciones con diferentes valores Either

ejemplo_convertir1 = convertirAInt (Left 42) -- 42

ejemplo_convertir2 = convertirAInt (Right True) -- 1

ejemplo_convertir3 = convertirAInt (Right False) -- 0

ejemplo_extraer1 = extraerNumero (Left "error") -- -1

ejemplo_extraer2 = extraerNumero (Right 100) -- 100

ejemplo_char1 = charONumero (Left 'A') -- 65 (ASCII de 'A')

ejemplo_char2 = charONumero (Right 25) -- 25

ejemplo_suma1 = sumaONumero (Left [1, 2, 3, 4]) -- 10

ejemplo_suma2 = sumaONumero (Right 50) -- 50

ejemplo_redondear1 = redondearODevolver (Left 3.7) -- 4

ejemplo_redondear2 = redondearODevolver (Right 15) -- 15

-- Ejercicio 3
inverso :: Float -> Maybe Float
inverso 0 = Nothing
inverso x = Just (1 / x)

aEntero :: Either Int Bool -> Int
aEntero (Left numero) = numero
aEntero (Right False) = 0
aEntero (Right True) = 1

-- Ejercicio 4
limpiar :: String -> String -> String
limpiar [] ys = ys
limpiar (x : xs) ys
  | x `elem` ys = limpiar xs (filter (/= x) ys)
  | otherwise = limpiar xs ys

-- Otra opcion mejor hecha por chatgpt
limpiar2 :: String -> String -> String
limpiar2 xs = filter (`notElem` xs)
  
difPromedio :: [Float] -> [Float]
difPromedio [] = []
difPromedio xs = let prom = promedio xs
  in difPromedioAux xs prom


difPromedioAux :: [Float] -> Float -> [Float]
difPromedioAux [] _ = []
difPromedioAux (x:xs) prom = (x - prom) : difPromedioAux xs prom


sumatoria :: [Float] -> Float
sumatoria [] = 0
sumatoria (x:xs) = x + sumatoria xs

promedio :: [Float] -> Float
promedio x = sumatoria x / fromIntegral (length x)


-- otra manera usando map
difPromedio' :: [Float] -> [Float]
difPromedio' [] = []
difPromedio' xs = let prom = promedio xs 
  in map(\x -> x - prom) xs


todosIguales :: [Int] -> Bool
todosIguales [] = True
todosIguales (x:xs) = let a = x
  in todosIgualesAux xs a

todosIgualesAux :: [Int] -> Int -> Bool
todosIgualesAux [] _ = True
todosIgualesAux (x:xs) a
  | a == x = todosIgualesAux xs a
  | otherwise = False

-- otra manera
todosIguales' :: Eq a => [a] -> Bool
todosIguales' []  = True
todosIguales' (x:xs) = all (== x) xs

-- Ejercicio 5

data AB a = Nil | Bin (AB a) a (AB a) deriving (Show)

t0 :: AB Int
t0 = Nil

t1 :: AB Int
t1 = Bin Nil 5 Nil

t2 :: AB Int
t2 = Bin (Bin Nil 2 Nil) 5 (Bin Nil 3 Nil)

t3 :: AB Int
t3 = Bin (Bin (Bin Nil 4 Nil) 2 (Bin Nil 40 Nil)) 5 (Bin (Bin Nil 32 Nil) 3 Nil)

t4 :: AB Bool
t4 = Bin (Bin Nil True Nil) False (Bin Nil True Nil)

vacioAB :: AB a -> Bool
vacioAB Nil = True
vacioAB (Bin {}) = False

negacionAB :: AB Bool -> AB Bool
negacionAB Nil = Nil
negacionAB (Bin izq valor der) = Bin (negacionAB izq) (not valor) (negacionAB der)

productoAB :: AB Int -> Int
productoAB Nil = 1
productoAB (Bin izq valor der) = valor * productoAB izq * productoAB der


-- para entender que es la currificacion, sumar y sumar2 son lo mismo
-- solo que sumar tiene azucar sintactico en los tipos, pero toda funcion
-- en haskell esta currificada
sumar :: Int -> Int -> Int
sumar x y = x + y

sumar2 :: Int -> (Int -> Int)
sumar2 x y = x + y

resta :: Int -> Int -> Int
resta x y = x - y

-- practica de tipos

h :: Bool -> Int -> String
h True n = "El numero es: " ++ show n
h False n = "No quiero mostrar el numero"

sumarUno = (+) 1

flip' :: (t1 -> t2 -> t3) -> t2 -> t1 -> t3
flip' f x y = f y x

const' :: a -> b -> a
const' x y = x

-- Funcion infinita
loop :: Int -> a
loop n = loop (n + 1)

-- abstracciones usando map
duplicaLista :: [Int] -> [Int]
duplicaLista = map (* 2)

esParL :: (Integral a) => [a] -> [Bool]
esParL xs = map (\x -> mod x 2 == 0) xs

longitudL :: [[a]] -> [Int]
longitudL xs = map length xs

-- otra manera
esParL' :: [Integer] -> [Bool]
esParL' = map ((== 0) . mod 2)

longitudL' :: [[a]] -> [Int]
longitudL' = map length


--abstraemos este patron con filter
negativos :: [Int] -> [Int]
negativos [] = []
negativos (x : xs) 
  | x < 0 = x : negativos xs
  | otherwise = negativos xs

noVacias :: [[a]] -> [[a]]
noVacias [] = []
noVacias (x : xs) = if not (null x) then x : noVacias xs else noVacias xs

--abstraccion con filter

negativos' = filter (< 0)

noVacias' :: [[a]] -> [[a]]
noVacias' = filter (not . null)


sucesor :: Int -> Int
sucesor x = x + 1
