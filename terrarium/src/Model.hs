module Model where

import JsonModel

verifyRectangularPotCal :: RectangularPotCalInput -> Maybe RectangularPotCalInput
verifyRectangularPotCal (RectangularPotCalInput radius length width)
    | radius <= 0 || length <= 0 || width <= 0  = Nothing
    | otherwise                 = Just (RectangularPotCalInput radius length width)

circlesInRectangle :: RectangularPotCalInput -> Int
circlesInRectangle (RectangularPotCalInput radius length width) = (floor (length / (radius * 2))) * (floor (width / (radius * 2)))

circlesInRectangleCoord :: RectangularPotCalInput -> [(Double, Double)]
circlesInRectangleCoord (RectangularPotCalInput radius length width) = [(i,j) | i <- takeWhile (<= length - radius) $ map (\x -> radius + (x * radius * 2)) [0..],
                                                                                j <- takeWhile (<= width - radius) $ map (\x -> radius + (x * radius * 2)) [0..]]
