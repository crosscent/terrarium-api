module TestModel where

import JsonModel
import Model
import Test.QuickCheck

testRectangularPotCal :: Double -> Double -> Double -> Bool
testRectangularPotCal x y z = (fmap circlesInRectangle cal) == (fmap length $ fmap circlesInRectangleCoord cal)
    where cal = verifyRectangularPotCal (RectangularPotCalInput x y z)
