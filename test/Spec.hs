import Test.HUnit hiding (Testable)
import Test.QuickCheck
import System.Random (newStdGen, randomR, StdGen)
import CardTest
import OthelloTest

othello_tests = TestList [testIsValidPos, testFlipLine, testFlipDisc, testIsPlayablePos, 
                  testCheckGameOverTrue, testCheckGameOverFalse]

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

    putStrLn "All Card Game tests completed!"

    putStrLn "Running Othello HUnit tests..."
    _ <- runTestTT othello_tests
    putStrLn "All Othello HUnit tests completed!"
