import Raytrace
import Test.QuickCheck

instance (Arbitrary a, Num a) => Arbitrary (Matrix a) where
  arbitrary = fmap Matrix $ vectorOf 4 $ vectorOf 4 arbitrary

prop_matrixTranspose m = matrixTranspose (matrixTranspose m) == m
