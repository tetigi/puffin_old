module Puffin.Math.Matrix(
  -- * Types
  Matrix(..),
  -- * Functions
  vectorDot,
  vectorScale,
  vectorAppend,
  vectorNormalize,
  vectorSize,
  negVector,
  listToVector,
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
import qualified Data.Vector as V

-- -------------------------------
-- Vector

instance Num a => Num (V.Vector a) where
  (+) = V.zipWith (+)
  (-) = V.zipWith (-)
  (*) = V.zipWith (*)
  negate = V.map negate
  fromInteger x = V.fromList (repeat 0)
  abs m = m
  signum _ = 1

-- | Dot product on two vectors.
vectorDot :: (Num a) => V.Vector a -> V.Vector a -> a
vectorDot v1 v2 = V.sum $ V.zipWith (*) v1 v2

-- | Scales a vector by a scalar.
vectorScale :: (Num a) => V.Vector a -> a -> V.Vector a
vectorScale v r = V.map (r*) v

-- | Appends a value to the end of a vector.
vectorAppend :: V.Vector a -> a -> V.Vector a
vectorAppend v x = v V.++ (V.fromList [x])

-- | Normalizes a vectors values, so that the total size is 1.
vectorNormalize :: (Num a, Fractional a, Floating a) => V.Vector a -> V.Vector a
vectorNormalize v = V.map (/ (vectorSize v)) v

-- | Calculates the size of a vector.
vectorSize :: (Floating a) => V.Vector a -> a
vectorSize v = sqrt . V.sum $ V.map (^2) v

-- | Negates a 4 length vector, graphics styleeeee
negVector :: (Num a) => V.Vector a -> V.Vector a
negVector v = (V.map negate (V.init v)) V.++ (V.fromList [V.last v])

listToVector :: [a] -> V.Vector a
listToVector = V.fromList

-- -------------------------------
-- Matrix TODO Convert to vector of vectors

newtype Matrix a = Matrix (V.Vector (V.Vector a)) deriving (Eq, Show)

instance Num a => Num (Matrix a) where
  (Matrix m1) + (Matrix m2) = Matrix (V.zipWith (V.zipWith (+)) m1 m2)
  (Matrix m1) - (Matrix m2) = Matrix (V.zipWith (V.zipWith (-)) m1 m2)
  (Matrix m1) * (Matrix m2) =
    listToMatrix [[sum $ zipWith (*) a b | b <- transpose bs] | a <- as]
    where 
      as = matrixToList (Matrix m1)
      bs = matrixToList (Matrix m2)
  negate (Matrix m) = Matrix $ V.map (V.map negate) m
  fromInteger x = listToMatrix (iterate (0:) (fromInteger x : repeat 0))
  abs m = m
  signum _ = 1

-- | Converts a vector of size n into a [nx1] size matrix.
matrixFromVector :: V.Vector a -> Matrix a
matrixFromVector v = Matrix $ V.singleton v

-- | Chops a matrix into a single vector of the first column.
matrixToVector :: Matrix a -> V.Vector a
matrixToVector (Matrix m) = V.head m

-- | Transposes a given matrix.
matrixTranspose :: Matrix a -> Matrix a
matrixTranspose = listToMatrix . transpose . matrixToList

-- | Converts a matrix to a list of lists
matrixToList :: Matrix a -> [[a]]
matrixToList (Matrix m) = V.toList $ V.map V.toList m

-- | Converts a list of lists into a matrix
listToMatrix :: [[a]] -> Matrix a
listToMatrix = Matrix . V.fromList . map V.fromList

-- | Multiples a matrix by a vector.
matrixMultVector :: (Num a) => Matrix a -> V.Vector a -> V.Vector a
matrixMultVector m v = matrixToVector $ m * (matrixFromVector v)

-- | Creates a square identity matrix of size [nxn].
matrixId :: Int -> Matrix Double
matrixId n =  listToMatrix [replicate i 0 ++ [1] ++ replicate (n-i-1) 0 | i <- [0..n-1]]

-- | Sets the fourth column of a graphics matrix.
matrixSetTranslate :: (Num a) => V.Vector a -> Matrix a -> Matrix a
matrixSetTranslate v m = matrixAddColumn v $ matrixTrimTo 3 4 m

-- | Gets the fourth column of a graphics matrix.
matrixGetTranslate :: (Num a) => Matrix a -> V.Vector a
matrixGetTranslate = matrixGetColumn 3

-- | Trim a matrix of any size to be of size [xxy].
matrixTrimTo :: Int -> Int -> Matrix a -> Matrix a
matrixTrimTo x y (Matrix m) = Matrix $ V.map (V.take y) $ V.take x m

-- | Appends a new row to the end of a matrix.
matrixAddRow :: V.Vector a -> Matrix a -> Matrix a
matrixAddRow v m = matrixTranspose $ matrixAddColumn v $ matrixTranspose m

-- | Appends a new column to the end of a matrix.
matrixAddColumn :: V.Vector a -> Matrix a -> Matrix a
matrixAddColumn v (Matrix m) = Matrix $ m V.++ (V.fromList [v])

-- | Gets a specific row from a matrix.
matrixGetRow :: Int -> Matrix a -> V.Vector a
matrixGetRow i m = matrixGetColumn i $ matrixTranspose m

-- | Gets a specific column from a matrix.
matrixGetColumn :: Int -> Matrix a -> V.Vector a
matrixGetColumn i (Matrix m) = m V.! i
