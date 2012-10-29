module Puffin.Math.Matrix where

import Data.List (transpose)

-- -------------------------------
-- Vector

newtype Vector a = Vector [a] deriving (Eq, Show)
instance Num a => Num (Vector a) where
  Vector as + Vector bs = Vector (zipWith (+) as bs)
  Vector as - Vector bs = Vector (zipWith (-) as bs)
  Vector as * Vector bs = Vector (zipWith (*) as bs)
  negate (Vector as) = Vector (map negate as)
  fromInteger x = Vector (repeat 0)
  abs m = m
  signum _ = 1

vectorDot :: (Num a) => Vector a -> Vector a -> a
vectorDot (Vector v1) (Vector v2) = sum $ zipWith (*) v1 v2

vectorScale :: (Num a) => Vector a -> a -> Vector a
vectorScale (Vector vs) r = Vector (map (r*) vs)

vectorAppend :: Vector a -> a -> Vector a
vectorAppend (Vector v) x = Vector (v ++ [x])

vectorNormalize :: (Num a, Fractional a, Floating a) => Vector a -> Vector a
vectorNormalize v@(Vector vs) = Vector $ map (/ (vectorSize v)) vs

vectorSize :: (Floating a) => Vector a -> a
vectorSize (Vector v) = sqrt . sum $ map (^2) v

-- negates a 4 length vector, graphics styleeeee
negVector :: (Num a) => Vector a -> Vector a
negVector (Vector v) = Vector $ (map negate (init v)) ++ [last v]

-- -------------------------------
-- Matrix

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

matrixFromVector :: Vector a -> Matrix a
matrixFromVector (Vector v) = Matrix [[m] | m <- v]

matrixToVector :: Matrix a -> Vector a
matrixToVector (Matrix ms) = Vector (map head ms)

matrixTranspose :: Matrix a -> Matrix a
matrixTranspose (Matrix as) = Matrix (transpose as)

matrixMultVector :: (Num a) => Matrix a -> Vector a -> Vector a
matrixMultVector m v = matrixToVector $ m * (matrixFromVector v)

matrixId :: Int -> Matrix Double
matrixId n = Matrix [replicate i 0 ++ [1] ++ replicate (n-i-1) 0 | i <- [0..n-1]]

matrixSetTranslate :: (Num a) => Vector a -> Matrix a -> Matrix a
matrixSetTranslate v m = matrixAddColumn v $ matrixAddRow oldScale $ matrixTrimTo 3 3 m
  where oldScale = matrixGetRow 3 $ matrixTrimTo 3 4 m

matrixGetTranslate :: (Num a) => Matrix a -> Vector a
matrixGetTranslate = matrixGetColumn 3

matrixTrimTo :: Int -> Int -> Matrix a -> Matrix a
matrixTrimTo x y (Matrix as) = Matrix $ map (take x) $ take y as 

matrixAddRow :: Vector a -> Matrix a -> Matrix a
matrixAddRow (Vector v) (Matrix as) = Matrix (as ++ [v])

matrixAddColumn :: Vector a -> Matrix a -> Matrix a
matrixAddColumn v m = matrixTranspose $ matrixAddRow v $ matrixTranspose m

matrixGetRow :: Int -> Matrix a -> Vector a
matrixGetRow i (Matrix as) = Vector (as !! i)

matrixGetColumn :: Int -> Matrix a -> Vector a
matrixGetColumn i m = matrixGetRow i $ matrixTranspose m



