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
  | n == k && not(esPrimo n) = 0
  | n == k && esPrimo n = 1
  | esPrimo k && mod n k == 0 = 1 + cantDivisoresPrimosAux n (k + 1)
  | otherwise =  cantDivisoresPrimosAux n (k + 1)

-- //////////////////////////////////////////////////////////////////////////////////////


-- otra version de cantDivsoresPrimos mejor que aprovecha ventajas de haskell

-- Devuelve True si el numero es primo
esPrimo2 :: Integer -> Bool
esPrimo2 n = n > 1 && all(\k -> n `mod` k /= 0) [2..(floor. sqrt. fromIntegral) n]


-- Devuelve la cantidad de divisores primos de n
cantDivisoresPrimos2 :: Integer -> Integer
cantDivisoresPrimos2 n = fromIntegral . length $ filter (\k -> n `mod`  k == 0 && esPrimo2 k) [2..n]








