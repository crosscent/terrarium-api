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

type InnerRadius = Double
type OuterRadius = Double
type X0          = Double
type X1          = Double
type Y0          = Double
type Y1          = Double
type NumberOfCircles = Double
type Dist        = Double

data MakeCirclesParams = MakeCirclesParams InnerRadius OuterRadius NumberOfCircles Dist X0 X1 Y0 Y1

verifyCircularPotCal :: CircularPotCalInput -> Maybe CircularPotCalInput
verifyCircularPotCal (CircularPotCalInput innerRadius outerRadius)
    | innerRadius <= 0 || outerRadius <= 0      = Nothing
    | outerRadius < innerRadius                 = Nothing
    | otherwise                                 = Just (CircularPotCalInput innerRadius outerRadius)


makeCirclesParams :: InnerRadius -> OuterRadius -> MakeCirclesParams
makeCirclesParams inner outer
    | dist < 2 * inner  = MakeCirclesParams inner outer (no - 1) dist x0 x1 y0 y1
    | otherwise         = MakeCirclesParams inner outer no dist x0 x1 y0 y1
    where no = (realToFrac . floor) $ (2 * pi * outer) / (2 * inner)
          x0 = outer * cos( 0 * 2 * pi / no)
          y0 = outer * sin( 0 * 2 * pi / no)
          x1 = outer * cos( 1 * 2 * pi / no)
          y1 = outer * sin( 1 * 2 * pi / no)
          dist = (((x0 - x1) ^^ 2) + ((y0 - y1) ^^ 2)) ** 0.5

makeCircles :: MakeCirclesParams -> [(Double, Double)]
makeCircles (MakeCirclesParams inner outer no dist x0 x1 y0 y1)
    | outer - (2 * inner) >= inner  = circles ++ (makeCircles $ makeCirclesParams inner (outer - (2 * inner)))
    | outer > (2 * inner)           = circles ++ [(0, 0)]
    | otherwise                     = circles
    where calX = (\x -> outer * cos (x * 2 * pi / no))
          calY = (\y -> outer * sin (y * 2 * pi / no))
          circles = [x | x <- map (\x -> (calX x, calY x)) [0..no - 1]]

circlesInCircleCoord :: CircularPotCalInput -> [(Double, Double)]
circlesInCircleCoord (CircularPotCalInput innerRadius outerRadius)
  | outerRadius < innerRadius                 = []
  | outerRadius < 2 * innerRadius             = [(0, 0)]
  | otherwise                                 = makeCircles $ makeCirclesParams innerRadius (outerRadius - innerRadius)
