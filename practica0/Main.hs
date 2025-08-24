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
limpiar (x:xs) ys
  | x `elem` ys = limpiar xs (filter (/= x) ys)
  | otherwise = limpiar xs ys


-- Otra opcion mejor hecha por chatgpt
limpiar2 :: String -> String -> String
limpiar2 xs = filter (`notElem` xs)

sumar :: Int -> Int -> Int
sumar x y = x + y

sumar2 :: Int -> (Int -> Int)
sumar2 x y = x + y