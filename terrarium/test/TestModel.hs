module TestModel where

import JsonModel
import Model
import Test.QuickCheck

testSquarePotCal :: Double -> Double -> Double -> Bool
testSquarePotCal x y z = (fmap circlesInSquare cal) == (fmap length $ fmap circlesInSquareCoord cal)
    where cal = verifySquarePotCal (SquarePotCalInput x y z)
