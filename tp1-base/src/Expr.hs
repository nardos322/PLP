module Expr
  ( Expr (..),
    recrExpr,
    foldExpr,
    eval,
    armarHistograma,
    evalHistograma,
    mostrar,
  )
where

import Generador
import Histograma

-- | Expresiones aritméticas con rangos
data Expr
  = Const Float
  | Rango Float Float
  | Suma Expr Expr
  | Resta Expr Expr
  | Mult Expr Expr
  | Div Expr Expr
  deriving (Show, Eq)

-- recrExpr :: ... anotar el tipo ...
recrExpr :: (Float -> b) -> (Float -> Float -> b) -> (Expr -> b -> Expr -> b -> b) ->
  (Expr -> b -> Expr -> b -> b) ->  (Expr -> b -> Expr -> b -> b) -> (Expr -> b -> Expr -> b -> b) -> Expr -> b
recrExpr fConst fRango fSuma fResta fMult fDiv expr =
  case expr of
    Const x -> fConst x
    Rango a b -> fRango a b
    (Suma l r) -> fSuma l (rec l) r (rec r)
    (Resta l r) -> fResta l (rec l) r (rec r)
    (Mult l r) -> fMult l (rec l) r (rec r)
    (Div l r) -> fDiv l (rec l) r (rec r)
  where
    rec = recrExpr fConst fRango fSuma fResta fMult fDiv


-- foldExpr :: ... anotar el tipo ...
foldExpr :: (Float -> b) -> (Float -> Float -> b) -> (b -> b -> b) ->
  (b -> b -> b) -> (b -> b -> b) -> (b -> b -> b) -> Expr -> b
foldExpr fConst fRango fSuma fResta fMult fDiv expr =
  case expr of
    Const x -> fConst x
    Rango a b -> fRango a b
    Suma l r -> fSuma (rec l) (rec r)
    Resta l r -> fResta (rec l) (rec r)
    Mult l r -> fMult (rec l) (rec r)
    Div l r -> fDiv (rec l) (rec r)
  where
    rec = foldExpr fConst fRango fSuma fResta fMult fDiv

-- | Evaluar expresiones dado un generador de números aleatorios
eval :: Expr -> G Float
eval = foldExpr
  (\x gen -> (x, gen))  -- (Const x) devuelve x
  (\a b gen -> dameUno (a, b) gen)  -- (Rango a b) devuelve un número entre a y b
  (combinarG2 (+))
  (combinarG2 (-))
  (combinarG2 (*))
  (combinarG2 (/))

combinarG2 :: (a -> b -> c) -> G a -> G b -> G c
combinarG2 op ga gb gen0 = let (a, gen1) = ga gen0       -- correr el primero con el generador
                               (b, gen2) = gb gen1       -- correr el segundo con el generador actualizado
                           in (op a b, gen2)             -- combinar valores y devolver nuevo generador


-- | @armarHistograma m n f g@ arma un histograma con @m@ casilleros
-- a partir del resultado de tomar @n@ muestras de @f@ usando el generador @g@.
armarHistograma :: Int -> Int -> G Float -> G Histograma
armarHistograma m n f g = 
  let (xs, g') = muestra f n g
      r = rango95 xs
      h = histograma m r xs
  in (h, g')


-- | @evalHistograma m n e g@ evalúa la expresión @e@ usando el generador @g@ @n@ veces
-- devuelve un histograma con @m@ casilleros y rango calculado con @rango95@ para abarcar el 95% de confianza de los valores.
-- @n@ debe ser mayor que 0.
evalHistograma :: Int -> Int -> Expr -> G Histograma
evalHistograma m n expr = armarHistograma m n (eval expr)

-- Podemos armar histogramas que muestren las n evaluaciones en m casilleros.
-- >>> evalHistograma 11 10 (Suma (Rango 1 5) (Rango 100 105)) (genNormalConSemilla 0)
-- (Histograma 102.005486 0.6733038 [1,0,0,0,1,3,1,2,0,0,1,1,0],<Gen>)

-- >>> evalHistograma 11 10000 (Suma (Rango 1 5) (Rango 100 105)) (genNormalConSemilla 0)
-- (Histograma 102.273895 0.5878462 [239,288,522,810,1110,1389,1394,1295,1076,793,520,310,254],<Gen>)

-- | Mostrar las expresiones, pero evitando algunos paréntesis innecesarios.
-- En particular queremos evitar paréntesis en sumas y productos anidados.
mostrar :: Expr -> String
mostrar = error "completar ejercicio 11"

data ConstructorExpr = CEConst | CERango | CESuma | CEResta | CEMult | CEDiv
  deriving (Show, Eq)

-- | Indica qué constructor fue usado para crear la expresión.
constructor :: Expr -> ConstructorExpr
constructor (Const _) = CEConst
constructor (Rango _ _) = CERango
constructor (Suma _ _) = CESuma
constructor (Resta _ _) = CEResta
constructor (Mult _ _) = CEMult
constructor (Div _ _) = CEDiv

-- | Agrega paréntesis antes y después del string si el Bool es True.
maybeParen :: Bool -> String -> String
maybeParen True s = "(" ++ s ++ ")"
maybeParen False s = s
