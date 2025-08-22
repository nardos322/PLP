absoluto :: Int -> Int
absoluto x
  | x > 0 = x
  | otherwise = -x

menorDivisorDesde :: Integer -> Integer -> Integer
menorDivisorDesde n k
  | mod n k == 0 = k
  | n == k = n
  | otherwise = menorDivisorDesde n (k + 1)

menorDivisor :: Integer -> Integer
menorDivisor n = menorDivisorDesde n 2

esPrimo :: Integer -> Bool
esPrimo n = n == menorDivisor n

factorial :: Int -> Int
factorial 0 = 1
factorial 1 = 1
factorial n = n * factorial (n - 1)


