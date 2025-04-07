module Testing where
import TestCases
import Test.Grader.Tests
import Test.Grader.Core
import Test.Grader.Eval
import Test.Grader.Rubric
import Control.Monad.Extra
import Control.Monad.Trans.RWS

tempTest = assess "temp" 0 $ do
        check "temp" $ 1 == 1 `shouldBe` True

tree = describe "Project 5" $ do
        describe "Sprint One" $ do
                tempTest

runTests :: Int -> Bool -> IO ()
runTests verb force = do
        let a = runGrader tree
        format <- makeFormat verb force "projectDesc.yaml"
        runRWST a () format
        return ()
