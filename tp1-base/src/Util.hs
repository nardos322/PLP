module Util where
import Data.Foldable (Foldable(fold))

-- | @alinearDerecha n s@ agrega espacios a la izquierda de @s@ hasta que su longitud sea @n@.
-- Si @s@ ya tiene longitud @>= n@, devuelve @s@.
alinearDerecha :: Int -> String -> String
alinearDerecha n s = foldl (\acc _ -> ' ':acc) s (replicate faltan  ' ')
    where
        faltan  = max 0 (n - length s)



-- | Dado un índice y una función, actualiza el elemento en la posición del índice
-- aplicando la función al valor actual. Si el índice está fuera de los límites
-- de la lista, devuelve la lista sin cambios.
-- El primer elemento de la lista es el índice 0.
actualizarElem :: Int -> (a -> a) -> [a] -> [a]
actualizarElem n f xs = actualizarElem' f xs n

actualizarElem' :: (a -> a) -> [a] -> (Int -> [a])
actualizarElem' f = foldr (\x rec i -> 
    let cola = rec (i-1)
        cabeza = if i == 0 then f x else x
    in cabeza : cola ) (const [])

-- | infinito positivo (Haskell no tiene literal para +infinito)
infinitoPositivo :: Float
infinitoPositivo = 1 / 0

-- | infinito negativo (Haskell no tiene literal para -infinito)
infinitoNegativo :: Float
infinitoNegativo = -(1 / 0)
