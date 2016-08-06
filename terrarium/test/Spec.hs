import TestModel
import Test.QuickCheck

main :: IO ()
main = do
    putStrLn "Testing rectangular pot calculation..."
    quickCheck testRectangularPotCal
