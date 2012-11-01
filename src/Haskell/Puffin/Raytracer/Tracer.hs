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

data Material = Material { materialColor :: Color, materialReflection :: Double, materialTransparency :: Double } deriving Eq

-- -------------------------------
-- Scene

class Sceneable a where
  setSceneAtTime :: Double -> a
  getTime :: a -> Double
  getCamera :: a -> Camera
  getLights :: a -> [Light]
  getSceneIntersect :: a -> Ray -> Intersection

data Color = Color (Double, Double, Double, Double) deriving (Eq, Ord)

instance Num Color where
  (Color (x1,x2,x3,x4)) + (Color (y1,y2,y3,y4)) = Color (x1+y1,x2+y2,x3+y3,x4+y4)
  (Color (x1,x2,x3,x4)) * (Color (y1,y2,y3,y4)) = Color (x1*y1,x2*y2,x3*y3,x4*y4)
  (Color (x1,x2,x3,x4)) - (Color (y1,y2,y3,y4)) = Color (x1-y1,x2-y2,x3-y3,x4-y4)
  abs c = c
  signum c = 1
  fromInteger i = Color (fromInteger i, fromInteger i, fromInteger i, fromInteger i) 

colorScale :: Double -> Color -> Color
colorScale n (Color (r,g,b,a)) = Color (r*n, g*n, b*n, a)

colorToGDColor :: Color -> GD.Color
colorToGDColor (Color (r, g, b, a)) = 
  GD.rgba (min (floor (255 * r)) 255) (min (floor (255 * g)) 255) (min (floor (255 * b)) 255) (127 - (min (floor (127 * a)) 127))

materialDefault = Material (Color (0.5, 0.5, 0.5, 1)) 0 0

trace :: (Sceneable a) => a -> Ray -> Double -> Color
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

renderScene :: Sceneable a => a -> IO ()
renderScene scene =
  do
    img <- GD.newImage (2048, 2048)
    GD.fillImage (GD.rgba 2047 2047 2047 20) img

    pixels <- return [ (x, 2048 -1 -y, trace scene (matrixMultRay (cameraTransform cam) (initRay baseVector (Vector [(x * pixelWidth) - 0.5, (y * pixelHeight) - 0.5, 1, 0]))) traceDepth) | x <- [0..2047], y <- [0..2047]]

    sequence_ $ map (\(x, y, c) -> GD.setPixel (floor x, floor y) (colorToGDColor c) img) pixels
    
    GD.savePngFile ("img" ++ show (getTime scene) ++ ".png") img
  where
    traceDepth = 4
    cam = getCamera scene
    pixelWidth = 1.0/2048
    pixelHeight = 1.0/2048
    baseVector = Vector [0, 0, 0, 1]
