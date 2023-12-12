import Test.QuickCheck
import System.Random (newStdGen, randomR, StdGen)
import CardTest


runTest :: Testable prop => String -> prop -> IO ()
runTest name prop = do
    putStrLn $ "Running " ++ name ++ "..."
    quickCheckWith stdArgs { maxSuccess = 10000 } prop

main :: IO ()
main = do
    runTest "prop_select" prop_select
    runTest "prop_initCard" prop_initCard
    runTest "prop_compareCards" prop_compareCards
    runTest "prop_getCard " prop_getCard 
    runTest "prop_updateState" prop_updateState

    putStrLn "All tests completed!"
