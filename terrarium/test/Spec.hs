import TestModel
import Test.QuickCheck

main :: IO ()
main = do
    putStrLn "Testing square pot calculation..."
    quickCheck testSquarePotCal
