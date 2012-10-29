module Puffin.Raytracer.Tracer where
import qualified Graphics.GD as GD
import Puffin.Math.Matrix

clamp :: (Ord a) => a -> a -> a -> a
clamp a b v
  | v < a     = a
  | v > b     = b
  | otherwise = v

lerp :: (Num a, Fractional a) => a -> a -> a -> a
lerp a b v  = a * (1.0 - v) + v * b

-- -------------------------------
-- Ray

data Ray = Ray { rayOrigin :: Vector Double, rayDirection :: Vector Double}
  deriving (Eq, Show)

initRay :: Vector Double -> Vector Double -> Ray
initRay origin direction = Ray origin (vectorNormalize direction)

getRayPosition :: Ray -> Double -> Vector Double
getRayPosition r d = (rayOrigin r) + vectorScale (rayDirection r) d

matrixMultRay :: Matrix Double -> Ray -> Ray
matrixMultRay m r = Ray (matrixMultVector m (rayOrigin r)) (matrixMultVector m (rayDirection r))

-- -------------------------------
-- Intersection

data Intersection = Intersection {  intersectionPosition :: Vector Double,
                                    intersectionNormal :: Vector Double, 
                                    intersectionMaterial :: Maybe Material,
                                    intersectionRayParameter :: Double, 
                                    intersectionIntersected :: Bool} deriving Eq

class Intersectable a where
  intersect :: Ray -> a -> Intersection

instance Ord Intersection where
  compare i1 i2 = compare (intersectionRayParameter i1) (intersectionRayParameter i2)


-- -------------------------------
-- Camera

data Camera = Camera { cameraTransform :: Matrix Double, cameraNear :: Double, cameraFar :: Double }

-- -------------------------------
-- Light

data Light = Light { lightPosition :: Vector Double, lightIntensity :: Double }

-- -------------------------------
-- Material 

data Material = Material { materialColor :: Color } deriving Eq

-- -------------------------------
-- Scene

class Sceneable a where
  setSceneAtTime :: Double -> a
  getTime :: a -> Double
  getCamera :: a -> Camera
  getLights :: a -> [Light]
  getSceneIntersect :: a -> Ray -> Intersection

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
        then (r', g', b', 1)
        else (0, 0, 0, 1)
      where
        material = intersectionMaterial i
        (r, g, b, _) = case material of 
                  Just m  -> materialColor m
                  Nothing -> (0.5, 0.5, 0.5, 1)

        lightDir = lightPosition l - intersectionPosition i
        d = vectorSize lightDir
        attenuation = lightIntensity l / (d * d)
        lightDirNorm = vectorNormalize lightDir

        shadowRay = initRay ((intersectionPosition i) + (vectorScale (intersectionNormal i) 0.0001)) lightDirNorm
        r' = r * attenuation * max 0 (vectorDot lightDirNorm (intersectionNormal i))
        g' = g * attenuation * max 0 (vectorDot lightDirNorm (intersectionNormal i))
        b' = b * attenuation * max 0 (vectorDot lightDirNorm (intersectionNormal i))

renderScene :: Sceneable a => a -> IO ()
renderScene scene =
  do
    img <- GD.newImage (256, 256)
    GD.fillImage (GD.rgba 255 255 255 20) img

    pixels <- return [ (x, 256 -1 -y, trace scene (matrixMultRay (cameraTransform cam) (initRay baseVector (Vector [(x * pixelWidth) - 0.5, (y * pixelHeight) - 0.5, 1, 0]))) 0) | x <- [0..255], y <- [0..255]]

    sequence_ $ map (\(x, y, c) -> GD.setPixel (floor x, floor y) (colorToGDColor c) img) pixels
    
    GD.savePngFile ("img" ++ show (getTime scene) ++ ".png") img
  where
    cam = getCamera scene
    pixelWidth = 1.0/256
    pixelHeight = 1.0/256
    baseVector = Vector [0, 0, 0, 1]
