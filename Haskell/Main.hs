import Raytrace

main = do
  frames <- return (map setSceneAtTime [0..9] :: [Scene])
  sequence_ $ map renderScene frames
  
