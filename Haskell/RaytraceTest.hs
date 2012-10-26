import Raytrace
import Test.QuickCheck


prop_clamp = undefined
prop_lerp = undefined

-- --------------------------
-- Vector

--TODO Check for multiply/dot mixups

instance (Arbitrary a, Num a) => Arbitrary (Vector a) where
  arbitrary = fmap Vector $ vectorOf 4 arbitrary

prop_vectorDot = undefined
prop_vectorScale = undefined
prop_vectorAppend = undefined
prop_vectorNormalize = undefined
prop_vectorSize = undefined
prop_negVector = undefined

test_Vector :: (Num a, Eq a) => [(Vector a -> Bool)]
test_Vector = [ prop_vectorDot
              , prop_vectorScale
              , prop_vectorAppend
              , prop_vectorNormalize
              , prop_vectorSize
              , prop_negVector
              ]

-- --------------------------
-- Matrix

instance (Arbitrary a, Num a) => Arbitrary (Matrix a) where
  arbitrary = fmap Matrix $ vectorOf 4 $ vectorOf 4 arbitrary

prop_matrixTranspose m@(Matrix ms) = 
  matrixTranspose (matrixTranspose m) == m &&
  (head ms) == ((\(Matrix a) -> map head a) (matrixTranspose m))
prop_matMultVector = undefined
prop_matMultRay = undefined
prop_matrixId = undefined
prop_matrixSetTranslate = undefined
prop_matrixGetTranslate = undefined
prop_matrixTrimTo = undefined
prop_matrixAddRow = undefined
prop_matrixGetRow = undefined
prop_matrixGetColumn = undefined

test_Matrix = []

-- --------------------------
-- Ray

-- --------------------------
-- Intersection

runTests :: (Testable a) => [a] -> IO ()
runTests = sequence_ . (map quickCheck)
