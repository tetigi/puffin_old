import Data.Array

clamp :: Double -> Double -> Double -> Double
clamp a b v
  | v < a     = a
  | v > b     = b
  | otherwise = v

lerp :: Double -> Double -> Double -> Double
lerp a b v  = a * (1.0 - v) + v * b


-- -------------------------------
-- Vector
-- -------------------------------

newtype Vector a = Vector [a] deriving (Eq, Show)
instance Num a => Num (Vector a) where
  Vector as + Vector bs = Vector (zipWith (+) as bs)
  Vector as - Vector bs = Vector (zipWith (-) as bs)
  Vector as * Vector bs = sum $ zipWith (*) as bs
  negate (Vector as) = Vector (map negate as)
  fromInteger x = Vector (repeat 0)
  abs m = m
  signum _ = 1

-- -------------------------------
-- Matrix
-- -------------------------------

newtype Matrix a = Matrix [[a]] deriving (Eq, Show)

instance Num a => Num (Matrix a) where
  Matrix as + Matrix bs = Matrix (zipWith (zipWith (+)) as bs)
  Matrix as - Matrix bs = Matrix (zipWith (zipWith (-)) as bs)
  Matrix as * Matrix bs =
    Matrix [[sum $ zipWith (*) a b | b <- transpose bs] | a <- as]
  negate (Matrix as) = Matrix (map (map negate) as)
  fromInteger x = Matrix (iterate (0:) (fromInteger x : repeat 0))
  abs m = m
  signum _ = 1

matrixInvertSimple :: Matrix Double -> Matrix Double
matrixInvertSimple a = undefined

matrixTranspose :: Matrix a -> Matrix a
matrixTranspose (Matrix as) = Matrix (transpose as)

transpose :: [[a]] -> [[a]]
transpose as
  | (length . head) as == 0 = []
  | otherwise     = (map head as) : (transpose (map tail as))

--matrixGetTranslate :: Matrix -> Vector
--matrixGetTranslate a = undefined

--matrixSetTranslate :: Matrix -> Vector -> Matrix
--matrixSetTranslate a = undefined


-- -------------------------------
-- Ray
-- -------------------------------


data Ray (Vector a) (Vector a) = Pair { origin :: a, direction :: a }
