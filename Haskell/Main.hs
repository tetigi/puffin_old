import Raytrace

main = do
  sequence_ $ map renderFrame [0..9]
