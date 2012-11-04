module Puffin.Raytracer.Tracer (
  -- * Classes
  Intersectable(..),
  Sceneable(..),
  -- * Types
  Ray(..),
  Intersection(..),
  Camera(..),
  Light(..),
  Material(..),
  Color(..),
  -- * Functions
  clamp,
  lerp,
  initRay,
  getRayPosition,
  matrixMultRay,
  renderScene
  ) where

import qualified Graphics.GD as GD
import Data.Vector(Vector)
import Puffin.Math.Matrix

-- | Clamps a value v to be within the range of a and b.
clamp :: (Ord a) => a -> a -> a -> a
clamp a b v
  | v < a     = a
  | v > b     = b
  | otherwise = v

-- | Linearly interpolates a value between a and b for a value v [0 - 1].
lerp :: (Num a, Fractional a, Ord a) => a -> a -> a -> a
lerp a b v  = a * (1.0 - (clamp 0 1 v)) + (clamp 0 1 v) * b

-- -------------------------------
-- Ray

-- | A simple ray from an origin in a direction.
-- Should be created with initRay to avoid normalization problems.
data Ray = 
  Ray { rayOrigin :: Vector Double, rayDirection :: Vector Double}
  deriving (Eq, Show)

-- | Initiates a ray properly, normalizing its direction.
initRay :: Vector Double -> Vector Double -> Ray
initRay origin direction = Ray origin (vectorNormalize direction)

-- | Gets the current position of a ray by scaling its direction from the origin.
getRayPosition :: Ray -> Double -> Vector Double
getRayPosition r d = (rayOrigin r) + vectorScale (rayDirection r) d

-- | Multiplies a matrix by a ray, creating a new ray.
matrixMultRay :: Matrix Double -> Ray -> Ray
matrixMultRay m r = Ray (matrixMultVector m (rayOrigin r)) (matrixMultVector m (rayDirection r))

-- -------------------------------
-- Intersection

-- | The intersection of a ray and an object, detailing key information for tracing.
data Intersection = Intersection {  intersectionPosition :: Vector Double,
                                    intersectionNormal :: Vector Double, 
                                    intersectionMaterial :: Maybe Material,
                                    intersectionRayParameter :: Double, 
                                    intersectionIntersected :: Bool} deriving Eq

-- | Defines an intersectable object with a ray.
class Intersectable a where
  -- | Intersect a ray with an intersectable, returning the intersection information.
  intersect :: Ray -> a -> Intersection

instance Ord Intersection where
  compare i1 i2 = compare (intersectionRayParameter i1) (intersectionRayParameter i2)

-- -------------------------------
-- Camera

-- | A simple camera type, detailing its world transform and near/far views.
data Camera = Camera { cameraTransform :: Matrix Double, cameraNear :: Double, cameraFar :: Double }

-- -------------------------------
-- Light

-- | Details the position and intensity of an omni-directional light source.
data Light = Light { lightPosition :: Vector Double, lightIntensity :: Double }

-- -------------------------------
-- Material 

-- | A simple material type.
data Material = Material { materialColor :: Color, materialReflection :: Double, materialTransparency :: Double } deriving Eq

-- -------------------------------
-- Scene

-- | Defines a custom scene of objects and other things, and supplies the methods for intersecting the scene.
class Sceneable a where
  -- | Return the the animated parts of the scene at a certain time.
  setSceneAtTime :: Double -> a
  -- | Return the current time in a scene.
  getTime :: a -> Double
  -- | Return the scene camera.
  getCamera :: a -> Camera
  -- | Return the lights in the scene.
  getLights :: a -> [Light]
  -- | Intersects a ray with the scene, performing intersection checks on objects.
  -- Should call intersect on all intersectable objects.
  getSceneIntersect :: a -> Ray -> Intersection

-- | A simple rgba type. TODO Could be improved
data Color = Color (Double, Double, Double, Double) deriving (Eq, Ord)

instance Num Color where
  (Color (x1,x2,x3,x4)) + (Color (y1,y2,y3,y4)) = Color (x1+y1,x2+y2,x3+y3,x4+y4)
  (Color (x1,x2,x3,x4)) * (Color (y1,y2,y3,y4)) = Color (x1*y1,x2*y2,x3*y3,x4*y4)
  (Color (x1,x2,x3,x4)) - (Color (y1,y2,y3,y4)) = Color (x1-y1,x2-y2,x3-y3,x4-y4)
  abs c = c
  signum c = 1
  fromInteger i = Color (fromInteger i, fromInteger i, fromInteger i, fromInteger i) 

-- | Scales a color intensity.
colorScale :: Double -> Color -> Color
colorScale n (Color (r,g,b,a)) = Color (r*n, g*n, b*n, a)

-- | Converts a Color type to the GD.Color type.
-- Color operates between 0 and 1 for intensity - GD.Color operates between 0 and 255 (and 127 for alpha).
colorToGDColor :: Color -> GD.Color
colorToGDColor (Color (r, g, b, a)) = 
  GD.rgba (min (floor (255 * r)) 255) (min (floor (255 * g)) 255) (min (floor (255 * b)) 255) (127 - (min (floor (127 * a)) 127))

-- | The default material if not specified in the object.
materialDefault = Material (Color (0.5, 0.5, 0.5, 1)) 0 0

-- | Trace a ray through a scene.
trace :: (Sceneable a) => 
  a         -- ^ The scene to trace through .
  -> Ray    -- ^ The ray to trace.
  -> Double -- ^ The depth to trace to. 
  -> Color  -- ^ The returned color values.
trace _ _ 0 = Color (0, 0, 0, 0)
trace s r d =
  if intersectionIntersected intersection
    then foldl1 sumColors $ ambient ++ shade ++ reflection -- ++ transparency
    else Color (0, 0, 0, 0)
  where
    intersection = getSceneIntersect s r

    ambient = [Color (0.1,0.1,0.1,1)]
    shade   = map (getLightIntersect s intersection) $ getLights s
    reflection = [colorScale (clamp 0 1 (materialReflection material)) (trace s reflectedRay (d-1))]
    transparency = [colorScale (clamp 0 1 (materialTransparency material)) (trace s transmittedRay (d-1))]

    material =  case intersectionMaterial intersection of
                Just m -> m
                Nothing -> materialDefault

    reflectedRay = 
      initRay (intersectionPosition intersection) reflectedDir
    reflectedDir = rayDirection r + vectorScale (intersectionNormal intersection) (2 * (vectorDot (intersectionNormal intersection) (- rayDirection r)))
    transmittedRay = undefined

    sumColors :: Color -> Color -> Color
    sumColors (Color (r1, g1, b1, _)) (Color (r2, g2, b2, _)) = Color (r1 + r2, g1 + g2, b1 + b2, 1)

    
    getLightIntersect :: (Sceneable a) => a -> Intersection -> Light -> Color
    getLightIntersect s i l =
      if not $ intersectionIntersected $ getSceneIntersect s shadowRay
        then Color(r', g', b', 1)
        else Color(0, 0, 0, 1)
      where
        material = intersectionMaterial i
        Color (r, g, b, _) = case material of 
                  Just m  -> materialColor m
                  Nothing -> Color (0.5, 0.5, 0.5, 1)

        lightDir = lightPosition l - intersectionPosition i
        d = vectorSize lightDir
        attenuation = lightIntensity l / (d * d)
        lightDirNorm = vectorNormalize lightDir

        shadowRay = initRay ((intersectionPosition i) + (vectorScale (intersectionNormal i) 0.0001)) lightDirNorm
        r' = r * attenuation * max 0 (vectorDot lightDirNorm (intersectionNormal i))
        g' = g * attenuation * max 0 (vectorDot lightDirNorm (intersectionNormal i))
        b' = b * attenuation * max 0 (vectorDot lightDirNorm (intersectionNormal i))

-- | Renders a whole scene, saving the output as a PNG.
renderScene :: Sceneable a => a -> IO ()
renderScene scene =
  do
    img <- GD.newImage (floor width, floor height)
    GD.fillImage (GD.rgba 255 255 255 20) img

    pixels <- return [ (x, height -1 -y, trace scene (matrixMultRay (cameraTransform cam) (initRay baseVector (listToVector [(x * pixelWidth) - 0.5, (y * pixelHeight) - 0.5, 1, 0]))) traceDepth) | x <- [0..width-1], y <- [0..height-1]]

    sequence_ $ map (\(x, y, c) -> GD.setPixel (floor x, floor y) (colorToGDColor c) img) pixels
    
    GD.savePngFile ("img" ++ show (getTime scene) ++ ".png") img
  where
    traceDepth = 4
    
    width :: Double
    height :: Double
    (width, height) = (512, 512)

    cam = getCamera scene
    pixelWidth = 1.0/width
    pixelHeight = 1.0/height
    baseVector = listToVector [0, 0, 0, 1]
