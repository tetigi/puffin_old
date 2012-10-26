module Raytrace where

import qualified Graphics.GD as GD

clamp :: (Ord a) => a -> a -> a -> a
clamp a b v
  | v < a     = a
  | v > b     = b
  | otherwise = v

lerp :: (Num a, Fractional a) => a -> a -> a -> a
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

{-matrixInvertSimple :: (Num a) => Matrix a -> Matrix a
matrixInvertSimple a = addColumn invTranslation $ trimTo 3 4 blankTranslation
  where
    oldScale = getRow 3 $ trimTo 3 4 a
    translation = matrixGetTranslate a
    threeTranspose = addColumn translation $ addRow oldScale $ matrixTranspose $ trimTo 3 3 a
    blankTranslation = addColumn (Vector [0,0,0,1]) $ trimTo 3 4 threeTranspose
    invTranslation = matMultVector blankTranslation $ negVector translation
-}
matrixTranspose :: Matrix a -> Matrix a
matrixTranspose (Matrix as) = Matrix (transpose as)

matMultVector :: (Num a) => Matrix a -> Vector a -> Vector a
matMultVector (Matrix ms) v = Vector [vectorDot v (Vector m) | m <- ms]

matMultRay :: Matrix Double -> Ray -> Ray
matMultRay m r = Ray (matMultVector m (rayOrigin r)) (matMultVector m (rayDirection r))

matrixId :: Int -> Matrix Double
matrixId n = Matrix [replicate i 0 ++ [1] ++ replicate (n-i-1) 0 | i <- [0..n-1]]

matrixSetTranslate :: (Num a) => Vector a -> Matrix a -> Matrix a
matrixSetTranslate v m = addColumn v $ addRow oldScale $ trimTo 3 3 m
  where oldScale = getRow 3 $ trimTo 3 4 m

matrixGetTranslate :: (Num a) => Matrix a -> Vector a
matrixGetTranslate = getColumn 3

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

-- -------------------------------
-- Ray

data Ray = Ray { rayOrigin :: Vector Double, rayDirection :: Vector Double}

getRayPosition :: Ray -> Double -> Vector Double
getRayPosition r d = (rayOrigin r) + vectorScale (rayDirection r) d

-- -------------------------------
-- Intersection

data Intersection = Intersection { intersectionPosition :: Vector Double, intersectionNormal :: Vector Double, intersectionRayParameter :: Double, intersectionIntersected :: Bool} deriving Eq

class Intersectable a where
  intersect :: Ray -> a -> Intersection

instance Ord Intersection where
  compare i1 i2 = compare (intersectionRayParameter i1) (intersectionRayParameter i2)

-- -------------------------------
-- Sphere

data Sphere = Sphere { spherePosition :: Vector Double, sphereRadius :: Double }

instance Intersectable Sphere where
  intersect ray@(Ray rayOrigin dir) s@(Sphere spherePosition sphereRadius) =
    if temp < 0.0 || intersectionRayParameter < 0.0 
      then Intersection undefined undefined undefined False
      else Intersection rayPos intersectionNormal intersectionRayParameter True
    where
      p = spherePosition - rayOrigin
      pDotRayDir = vectorDot p dir
      radiusSq = sphereRadius * sphereRadius

      temp = radiusSq + pDotRayDir * pDotRayDir - (vectorDot p p)
      
      intersectionRayParameter = pDotRayDir - sqrt temp

      rayPos = getRayPosition ray intersectionRayParameter
      intersectionNormal = vectorNormalize (rayPos - spherePosition)
      
    

-- -------------------------------
-- Plane

data Plane = Plane { planeNormal :: Vector Double, planeDistance :: Double }

tolerance = 1e-12

instance Intersectable Plane where
  intersect  ray@(Ray rayOrigin dir) p@(Plane planeNormal planeDistance) =
    if (d < tolerance && d > -tolerance) || intersectionRayParameter < 0.0
      then Intersection undefined undefined undefined False
      else Intersection rayPos planeNormal intersectionRayParameter True
    where
      d = vectorDot planeNormal dir
      intersectionRayParameter = (planeDistance - vectorDot planeNormal rayOrigin) / d
      rayPos = getRayPosition ray intersectionRayParameter


-- -------------------------------
-- Camera

data Camera = Camera { cameraTransform :: Matrix Double, cameraNear :: Double, cameraFar :: Double }

-- -------------------------------
-- Light

data Light = Light { lightPosition :: Vector Double, lightIntensity :: Double }

-- -------------------------------
-- Scene

data Scene = Scene { sceneTime :: Double, sceneSpheres :: [Sphere], scenePlanes :: [Plane], sceneLights :: [Light], sceneCamera :: Camera }

class Sceneable a where
  setSceneAtTime :: Double -> a
  getCamera :: a -> Camera
  getLights :: a -> [Light]
  getSceneIntersect :: a -> Ray -> Intersection

instance Sceneable Scene where
  setSceneAtTime sceneTime = 
    Scene sceneTime nSpheres nPlanes nLights nCamera
    where
      nTime = clamp 0 10 sceneTime
      a = nTime / 10
      d = lerp 10 2 a
      z = lerp 10 5 a
      h = lerp 1 5 a
      
      nSpheres  = [Sphere (Vector [0, 1, 20, 1]) 1
                  ,Sphere (Vector [-d, 1, 20, 1]) 1
                  ,Sphere (Vector [d, 1, 20, 1]) 1]
      nPlanes   = [Plane (Vector [0, 1, 0, 0]) 0]
      nLights   = [Light (Vector [0, 4, z, 1]) 15]
     
      m = matrixSetTranslate (Vector [0, h, 0, 1]) $ matrixId 4
      nCamera = Camera m 1 50

  getCamera = sceneCamera

  getSceneIntersect s r = 
    if length successful > 0
      then minimum successful
      else Intersection undefined undefined undefined False
    where
      sphereIntersects = map (intersect r) $ sceneSpheres s
      planeIntersects = map (intersect r) $ scenePlanes s
      successful = filter intersectionIntersected (sphereIntersects ++ planeIntersects)

  getLights = sceneLights

type Color = (Double, Double, Double, Double)
colorToGDColor :: Color -> GD.Color
colorToGDColor (r, g, b, a) = 
  GD.rgba (min (floor (255 * r)) 255) (min (floor (255 * g)) 255) (min (floor (255 * b)) 255) (127 - (min (floor (127 * a)) 127))

trace :: (Sceneable a) => a -> Ray -> Double -> Color
trace s r d =
  if intersectionIntersected intersection
    then foldl1 sumColors $ map (getLightIntersect s intersection) $ getLights s
    else (0, 0, 0, 0)
  where
    sumColors :: Color -> Color -> Color
    sumColors (r1, g1, b1, _) (r2, g2, b2, _) = (r1 + r2, g1 + g2, b1 + b2, 1)

    intersection = getSceneIntersect s r
    
    getLightIntersect :: (Sceneable a) => a -> Intersection -> Light -> Color
    getLightIntersect s i l =
      if not $ intersectionIntersected $ getSceneIntersect s shadowRay
        then (a, a, a, 1)
        else (0, 0, 0, 1)
      where
        lightDir = lightPosition l - intersectionPosition i
        d = vectorSize lightDir
        attenuation = lightIntensity l / (d * d)
        lightDirNorm = vectorNormalize lightDir

        shadowRay = Ray ((intersectionPosition i) + (vectorScale (intersectionNormal i) 0.001)) lightDirNorm
        a = attenuation * max 0 (vectorDot lightDir (intersectionNormal i))

renderFrame :: Integer -> IO ()
renderFrame i =
  do
    img <- GD.newImage (256, 256)
    GD.fillImage (GD.rgb 0 0 0) img
    pixels <- return [ (x, 256 -1 -y, trace scene (matMultRay (cameraTransform cam) (Ray baseVector (Vector [x * pixelWidth - 0.5, y * pixelHeight - 0.5, 1, 0]))) 0) | x <- [0..255], y <- [0..255]]
    sequence_ $ map (\(x, y, c) -> GD.setPixel (floor x, floor y) (colorToGDColor c) img) pixels
    GD.savePngFile ("img" ++ show i ++ ".png") img
  where
    scene :: Scene
    scene = setSceneAtTime (fromInteger i)
    cam = getCamera scene
    pixelWidth = 1.0/256
    pixelHeight = 1.0/256
    baseVector = Vector [0, 0, 0, 1]

--TODO Change accessors to gets
