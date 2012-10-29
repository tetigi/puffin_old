module Main where

import Puffin.Math.Matrix
import Puffin.Raytracer.Tracer
import Puffin.Raytracer.Objects

data Scene = Scene { sceneTime :: Double, sceneSpheres :: [Object], scenePlanes :: [Plane], sceneLights :: [Light], sceneCamera :: Camera }

instance Sceneable Scene where
  getTime = sceneTime
  setSceneAtTime sceneTime = 
    Scene sceneTime nSpheres nPlanes nLights nCamera
    where
      nTime = clamp 0 10 sceneTime
      a = nTime / 10.0
      d = lerp 10.0 2.0 a
      z = lerp 10.0 20.0 a
      h = lerp 1.0 5.0 a
     
      
      nSpheres  = [Object (Sphere (Vector [0, 1, 20, 1]) 1) (Material (1, 0, 0, 1))
                  ,Object (Sphere (Vector [-d, 1, 20, 1]) 1) (Material (0, 1, 0, 1))
                  ,Object (Sphere (Vector [d, 1, 20, 1]) 1) (Material (0, 0, 1, 1))]
      nPlanes   = [Plane (Vector [0, 1, 0, 0]) 0
                  ]
      nLights   = [Light (Vector [0, 4, z, 1]) 15]
      
      m = matrixSetTranslate (Vector [0, h, 0, 1.0]) $ matrixId 4
      nCamera = Camera m 1 50

  getCamera = sceneCamera

  getSceneIntersect s r = 
    if length successful > 0
      then minimum successful
      else Intersection undefined undefined undefined undefined False
    where
      sphereIntersects = map (intersect r) $ sceneSpheres s
      planeIntersects = map (intersect r) $ scenePlanes s
      successful = filter intersectionIntersected (sphereIntersects ++ planeIntersects)

  getLights = sceneLights

main = do
  frames <- return (map setSceneAtTime [0..9] :: [Scene])
  sequence_ $ map renderScene frames
  
