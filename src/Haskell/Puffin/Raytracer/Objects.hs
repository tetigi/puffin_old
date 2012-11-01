module Puffin.Raytracer.Objects(
  -- * Types
  Object(..),
  Sphere(..),
  Disk(..),
  Plane(..)
  )where

import Puffin.Raytracer.Tracer
import Puffin.Math.Matrix
-- -------------------------------
-- Object

-- | A general data type for any object, consisting of an intersectable shape and a material for the object.
data Object = forall a. Intersectable a => Object { objectShape :: a, objectMaterial :: Material }

instance Intersectable Object where
  intersect ray Object{ objectShape = o, objectMaterial = m } = 
    (intersect ray o){ intersectionMaterial = Just m }

-- -------------------------------
-- Sphere

-- | A primitive sphere type.
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
-- Segmented plane

-- | A primitive disk shape.
data Disk = Disk { diskCenter :: Vector Double, diskNormal :: Vector Double, diskWidth :: Double }

instance Intersectable Disk where
  intersect  ray@(Ray origin dir) p@(Disk center normal width) =
    if (d < tolerance && d > -tolerance) || intersectionRayParameter < 0.0 || distance > width
      then Intersection undefined undefined undefined undefined False
      else Intersection rayPos (vectorNormalize normal) Nothing intersectionRayParameter True
    where
      d = vectorDot normal dir
      intersectionRayParameter = ((vectorDot normal center) - (vectorDot normal origin)) / d
      rayPos = getRayPosition ray intersectionRayParameter
      distance = vectorSize (rayPos - center)

-- -------------------------------
-- Plane

-- | A primitive plane of unlimited size
data Plane = Plane { planeNormal :: Vector Double, planeDistance :: Double }

-- | Error margin.
tolerance = 1e-12

instance Intersectable Plane where
  intersect  ray@(Ray origin dir) p@(Plane normal distance) =
    if (d < tolerance && d > -tolerance) || intersectionRayParameter < 0.0
      then Intersection undefined undefined undefined undefined False
      else Intersection rayPos (vectorNormalize normal) Nothing intersectionRayParameter True
    where
      d = vectorDot normal dir
      intersectionRayParameter = ((vectorDot normal (vectorScale normal distance)) - (vectorDot normal origin)) / d
      rayPos = getRayPosition ray intersectionRayParameter


