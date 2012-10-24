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

vectorNormalize :: (Num a, Fractional a) => Vector a -> Vector a
vectorNormalize (Vector v) = Vector $ map (/ (sum v)) v

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

matrixInvertSimple :: (Num a) => Matrix a -> Matrix a
matrixInvertSimple a = addColumn invTranslation $ trimTo 3 4 blankTranslation
  where
    oldScale = getRow 3 $ trimTo 3 4 a
    translation = getColumn 3 a
    threeTranspose = addColumn translation $ addRow oldScale $ matrixTranspose $ trimTo 3 3 a
    blankTranslation = addColumn (Vector [0,0,0,1]) $ trimTo 3 4 threeTranspose
    invTranslation = matMultVector blankTranslation $ negVector translation

matrixTranspose :: Matrix a -> Matrix a
matrixTranspose (Matrix as) = Matrix (transpose as)

matMultVector :: (Num a) => Matrix a -> Vector a -> Vector a
matMultVector (Matrix ms) v = Vector [vectorDot v (Vector m) | m <- ms]

transpose :: [[a]] -> [[a]]
transpose as 
  | (length . head) as == 0 = []
  | otherwise     = (map head as) : (transpose (map tail as))

trimTo :: Int -> Int -> Matrix a -> Matrix a
trimTo x y (Matrix as) = Matrix $ map (take x) $ take y as 

addRow :: Vector a -> Matrix a -> Matrix a
addRow (Vector v) (Matrix as) = Matrix (as ++ [v])

addColumn :: Vector a -> Matrix a -> Matrix a
addColumn v m = matrixTranspose $ addRow v $ matrixTranspose m

getRow :: Int -> Matrix a -> Vector a
getRow i (Matrix as) = Vector (as !! i)

getColumn :: Int -> Matrix a -> Vector a
getColumn i m = getRow i $ matrixTranspose m

--matrixGetTranslate :: Matrix -> Vector
--matrixGetTranslate a = undefined

--matrixSetTranslate :: Matrix -> Vector -> Matrix
--matrixSetTranslate a = undefined


-- -------------------------------
-- Ray

data Ray = Ray { origin :: Vector Double, direction :: Vector Double}

getRayPosition :: Ray -> Double -> Vector Double
getRayPosition r d = (origin r) + vectorScale (direction r) d

-- -------------------------------
-- Intersection

data Intersection = Intersection { intersectionPos :: Vector Double, normal :: Vector Double, rayParameter :: Double, intersected :: Bool}

class Intersectable a where
  intersect :: a -> Ray -> Intersection

-- -------------------------------
-- Sphere

data Sphere = Sphere { spherePos :: Vector Double, radius :: Double }

instance Intersectable Sphere where
  intersect s@(Sphere position radius) ray@(Ray origin dir) = undefined
    where
      p = position - origin
      pDotRayDir = vectorDot p dir
      radiusSq = radius * radius

      temp = radiusSq + pDotRayDir * pDotRayDir - (vectorDot p p)
      
      rayParameter = pDotRayDir - sqrt temp

      position = getRayPosition ray rayParameter
      
    

-- -------------------------------
-- Plane

data Plane = Plane { planeNorm :: Vector Double, distance :: Double }

instance Intersectable Plane where
  intersect (Plane n1 d1) (Ray origin dir) = undefined

-- -------------------------------
-- Camera

data Camera = Camera { transform :: Matrix Double, near :: Double, far :: Double }

-- -------------------------------
-- Light

data Light = Light { position :: Vector Double, intensity :: Double }

-- -------------------------------
-- Scene

data Scene = Scene { time :: Double, spheres :: [Sphere], planes :: [Plane], lights :: [Light] }

getSceneIntersect :: Scene -> Ray -> Intersection
getSceneIntersect = undefined

type Color = (Double, Double, Double, Double)

trace :: Scene -> Ray -> Double -> Color
trace = undefined
