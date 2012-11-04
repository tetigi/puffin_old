module Main where

import Puffin.Math.Matrix
import Puffin.Raytracer.Tracer
import Puffin.Raytracer.Objects

-- | A basic scene consisting of sphere and disks.
data Scene = Scene { sceneTime :: Double, sceneSpheres :: [Object], sceneDisks :: [Disk], sceneLights :: [Light], sceneCamera :: Camera }

instance Sceneable Scene where
  getTime = sceneTime
  setSceneAtTime sceneTime = 
    Scene sceneTime nSpheres nPlanes nLights nCamera
    where
      nTime = clamp 0 10 sceneTime
      a = nTime / 10.0
      d = lerp 10.0 2.0 a
      z = lerp 10.0 20.0 a
      h = lerp 5.0 5.0 a
     
      
      nSpheres  = [Object (Sphere (listToVector [0, 1, 20, 1]) 1) (Material (Color (1, 0, 0, 1)) 1 0)
                  ,Object (Sphere (listToVector [-d, 1, 20, 1]) 1) (Material (Color (0, 1, 0, 1)) 1 0)
                  ,Object (Sphere (listToVector [0, 1, 15, 1]) 1) (Material (Color (0, 1, 0, 1)) 1 0)
                  ,Object (Sphere (listToVector [0, 3, 18, 1]) 1) (Material (Color (0, 1, 0, 1)) 1 0)
                  ,Object (Sphere (listToVector [d, 1, 20, 1]) 1) (Material (Color (0, 0, 1, 1)) 1 0)]
      nPlanes   = [Disk (listToVector [0, 0, 20, 1]) (listToVector [0, 1, 0, 0]) 8
                  ,Disk (listToVector [0, 0, 23, 1]) (listToVector [0, 0, -1, 0]) 5
                   --,Plane (vectorNormalize (listToVector [0, 1, -1, 0])) (-15)
                  ]
      nLights   = [Light (listToVector [0, 4, z, 1]) 10
                  ,Light (listToVector [0, 8, 25, 1]) 50
                  ,Light (listToVector [15, 8, 20, 1]) 50
                  ,Light (listToVector [-15, 8, 20, 1]) 50]
      
      m = matrixSetTranslate (listToVector [0, h, 0, 1.0]) $ matrixId 4
      nCamera = Camera m 1 50

  getCamera = sceneCamera

  getSceneIntersect s r = 
    if length successful > 0
      then minimum successful
      else Intersection undefined undefined undefined undefined False
    where
      sphereIntersects = map (intersect r) $ sceneSpheres s
      planeIntersects = map (intersect r) $ sceneDisks s
      successful = filter intersectionIntersected (sphereIntersects ++ planeIntersects)

  getLights = sceneLights

main = do
  let frames = (map setSceneAtTime [9] :: [Scene])
  sequence_ $ map renderScene frames
  
