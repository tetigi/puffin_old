module Puffin.Raytracer.Objects where

import Puffin.Raytracer.Tracer
import Puffin.Math.Matrix
-- -------------------------------
-- Object

data Object = forall a. Intersectable a => Object { objectShape :: a, objectMaterial :: Material }

instance Intersectable Object where
  intersect ray Object{ objectShape = o, objectMaterial = m } = 
    (intersect ray o){ intersectionMaterial = Just m }

-- -------------------------------
-- Sphere

data Sphere = Sphere { spherePosition :: Vector Double, sphereRadius :: Double }

instance Intersectable Sphere where
  intersect ray@(Ray origin direction) s@(Sphere position radius) =
    if temp < 0.0 || intersectionRayParameter < 0.0 
      then Intersection undefined undefined undefined undefined False
      else Intersection rayPos intersectionNormal Nothing intersectionRayParameter True
    where
      p = position - origin
      pDotRayDir = vectorDot p direction
      radiusSq = radius * radius

      temp = radiusSq + (pDotRayDir * pDotRayDir) - (vectorDot p p)
      
      intersectionRayParameter = pDotRayDir - sqrt temp

      rayPos = getRayPosition ray intersectionRayParameter
      intersectionNormal = vectorNormalize (rayPos - position)
      
-- -------------------------------
-- Plane

data Plane = Plane { planeNormal :: Vector Double, planeDistance :: Double }

tolerance = 1e-12

instance Intersectable Plane where
  intersect  ray@(Ray rayOrigin dir) p@(Plane planeNormal planeDistance) =
    if (d < tolerance && d > -tolerance) || intersectionRayParameter < 0.0
      then Intersection undefined undefined undefined undefined False
      else Intersection rayPos planeNormal Nothing intersectionRayParameter True
    where
      d = vectorDot planeNormal dir
      intersectionRayParameter = (planeDistance - vectorDot planeNormal rayOrigin) / d
      rayPos = getRayPosition ray intersectionRayParameter


