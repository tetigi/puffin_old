module Puffin.Math.Matrix(
  -- * Types
  Vector(..),
  Matrix(..),
  -- * Functions
  vectorDot,
  vectorScale,
  vectorAppend,
  vectorNormalize,
  vectorSize,
  negVector,
  matrixFromVector,
  matrixToVector,
  matrixTranspose,
  matrixMultVector,
  matrixId,
  matrixSetTranslate,
  matrixGetTranslate,
  matrixTrimTo,
  matrixAddRow,
  matrixAddColumn,
  matrixGetRow,
  matrixGetColumn
  )where

import Data.List (transpose)

-- -------------------------------
-- Vector

-- | A vertical vector of any size.
newtype Vector a = Vector [a] deriving (Eq, Show)
instance Num a => Num (Vector a) where
  Vector as + Vector bs = Vector (zipWith (+) as bs)
  Vector as - Vector bs = Vector (zipWith (-) as bs)
  Vector as * Vector bs = Vector (zipWith (*) as bs)
  negate (Vector as) = Vector (map negate as)
  fromInteger x = Vector (repeat 0)
  abs m = m
  signum _ = 1

-- | Dot product on two vectors.
vectorDot :: (Num a) => Vector a -> Vector a -> a
vectorDot (Vector v1) (Vector v2) = sum $ zipWith (*) v1 v2

-- | Scales a vector by a scalar.
vectorScale :: (Num a) => Vector a -> a -> Vector a
vectorScale (Vector vs) r = Vector (map (r*) vs)

-- | Appends a value to the end of a vector.
vectorAppend :: Vector a -> a -> Vector a
vectorAppend (Vector v) x = Vector (v ++ [x])

-- | Normalizes a vectors values, so that the total size is 1.
vectorNormalize :: (Num a, Fractional a, Floating a) => Vector a -> Vector a
vectorNormalize v@(Vector vs) = Vector $ map (/ (vectorSize v)) vs

-- | Calculates the size of a vector.
vectorSize :: (Floating a) => Vector a -> a
vectorSize (Vector v) = sqrt . sum $ map (^2) v

-- | Negates a 4 length vector, graphics styleeeee
negVector :: (Num a) => Vector a -> Vector a
negVector (Vector v) = Vector $ (map negate (init v)) ++ [last v]

-- -------------------------------
-- Matrix

-- | A matrix of any size.
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

-- | Converts a vector of size n into a [nx1] size matrix.
matrixFromVector :: Vector a -> Matrix a
matrixFromVector (Vector v) = Matrix [[m] | m <- v]

-- | Chops a matrix into a single vector of the first column.
matrixToVector :: Matrix a -> Vector a
matrixToVector (Matrix ms) = Vector (map head ms)

-- | Transposes a given matrix.
matrixTranspose :: Matrix a -> Matrix a
matrixTranspose (Matrix as) = Matrix (transpose as)

-- | Multiples a matrix by a vector.
matrixMultVector :: (Num a) => Matrix a -> Vector a -> Vector a
matrixMultVector m v = matrixToVector $ m * (matrixFromVector v)

-- | Creates a square identity matrix of size [nxn].
matrixId :: Int -> Matrix Double
matrixId n = Matrix [replicate i 0 ++ [1] ++ replicate (n-i-1) 0 | i <- [0..n-1]]

-- | Sets the fourth column of a graphics matrix.
matrixSetTranslate :: (Num a) => Vector a -> Matrix a -> Matrix a
matrixSetTranslate v m = matrixAddColumn v $ matrixAddRow oldScale $ matrixTrimTo 3 3 m
  where oldScale = matrixGetRow 3 $ matrixTrimTo 3 4 m

-- | Gets the fourth column of a graphics matrix.
matrixGetTranslate :: (Num a) => Matrix a -> Vector a
matrixGetTranslate = matrixGetColumn 3

-- | Trim a matrix of any size to be of size [xxy].
matrixTrimTo :: Int -> Int -> Matrix a -> Matrix a
matrixTrimTo x y (Matrix as) = Matrix $ map (take x) $ take y as 

-- | Appends a new row to the end of a matrix.
matrixAddRow :: Vector a -> Matrix a -> Matrix a
matrixAddRow (Vector v) (Matrix as) = Matrix (as ++ [v])

-- | Appends a new column to the end of a matrix.
matrixAddColumn :: Vector a -> Matrix a -> Matrix a
matrixAddColumn v m = matrixTranspose $ matrixAddRow v $ matrixTranspose m

-- | Gets a specific row from a matrix.
matrixGetRow :: Int -> Matrix a -> Vector a
matrixGetRow i (Matrix as) = Vector (as !! i)

-- | Gets a specific column from a matrix.
matrixGetColumn :: Int -> Matrix a -> Vector a
matrixGetColumn i m = matrixGetRow i $ matrixTranspose m



