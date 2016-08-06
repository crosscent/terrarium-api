module Model where

import JsonModel

verifySquarePotCal :: SquarePotCalInput -> Maybe SquarePotCalInput
verifySquarePotCal (SquarePotCalInput radius length width)
    | radius <= 0 || length <= 0 || width <= 0  = Nothing
    | otherwise                 = Just (SquarePotCalInput radius length width)

circlesInSquare :: SquarePotCalInput -> Int
circlesInSquare (SquarePotCalInput radius length width) = (floor (length / (radius * 2))) * (floor (width / (radius * 2)))

circlesInSquareCoord :: SquarePotCalInput -> [(Double, Double)]
circlesInSquareCoord (SquarePotCalInput radius length width) = [(i,j) | i <- takeWhile (<= length - radius) $ map (\x -> radius + (x * radius * 2)) [0..],
                                                                        j <- takeWhile (<= width - radius) $ map (\x -> radius + (x * radius * 2)) [0..]]
