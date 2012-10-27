import Raytrace
import Test.QuickCheck
import Control.Applicative

testTolerance = 1e-5

(~=) :: Double -> Double -> Bool
(~=) target number = 
  rem <= testTolerance && rem >= -testTolerance
  where
    rem = target - number

prop_clamp = undefined
prop_lerp = undefined

-- --------------------------
-- Vector

--TODO Check for multiply/dot mixups

instance (Arbitrary a, Num a) => Arbitrary (Vector a) where
  arbitrary = fmap Vector $ vectorOf 4 arbitrary

prop_vectorDot v1 v2 = 
  vectorDot v1 v2 == vectorDot v2 v1 &&
  vectorDot v1 (v1 + v2) == vectorDot v1 v1 + vectorDot v1 v2 

prop_vectorAppend v@(Vector vs) n =
  (\(Vector v') -> length v') (vectorAppend v n) == length vs + 1

isZero :: (Num a, Eq a) => Vector a -> Bool
isZero (Vector vs) = and $ map (==0) vs

prop_vectorNormalize v =
  not (isZero v) ==> vectorSize (vectorNormalize v) ~= 1

prop_negVector v@(Vector vs) = 
  negVector (negVector v) == v 

{-
test_Vector = [ quickCheck prop_vectorDot
              , quickCheck prop_vectorScale
              , quickCheck prop_vectorAppend
              , quickCheck prop_vectorNormalize
              , quickCheck prop_negVector
              ]
              -}
-- --------------------------
-- Matrix

instance (Arbitrary a, Num a) => Arbitrary (Matrix a) where
  arbitrary = fmap Matrix $ vectorOf 4 $ vectorOf 4 arbitrary

prop_matrixTranspose m@(Matrix ms) = 
  matrixTranspose (matrixTranspose m) == m &&
  (head ms) == ((\(Matrix a) -> map head a) (matrixTranspose m))

prop_matrixId n = 
  n > 0 && n < 100 ==>
    floor (sum [vectorSize (matrixGetRow i (matrixId n)) | i <- [0..n-1]]) == n

prop_matrixSetTranslate = undefined
prop_matrixGetTranslate = undefined
prop_matrixTrimTo = undefined
prop_matrixAddRow = undefined
prop_matrixGetRow = undefined
prop_matrixGetColumn = undefined

{-
test_Matrix = [ prop_matMultVector
              , prop_matMultRay
              , prop_matrixId
              , prop_matrixSetTranslate
              , prop_matrixGetTranslate
              , prop_matrixTrimTo
              , prop_matrixAddRow
              , prop_matrixGetRow
              , prop_matrixGetColumn
              ]
-}
-- --------------------------
-- Ray

instance Arbitrary Ray where
  arbitrary = Ray <$> arbitrary <*>  arbitrary

prop_getRayPosition = undefined

test_Ray = [ prop_getRayPosition ]

-- --------------------------
-- Sphere

instance Arbitrary Sphere where
  arbitrary = Sphere <$> arbitrary <*> arbitrary

prop_sphereIntersect = undefined

test_Sphere = [ prop_sphereIntersect ]

-- --------------------------
-- Plane

instance Arbitrary Plane where
  arbitrary = Plane <$> arbitrary <*> arbitrary

prop_planeIntersect = undefined

runTests :: (Testable a) => [a] -> IO ()
runTests = sequence_ . (map quickCheck)
