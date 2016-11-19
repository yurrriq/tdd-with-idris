module Exercise.Test.State

import Exercise.State

import Control.Monad.State
import Test.Helpers

%default total
%access export

testIncrease : IO ()
testIncrease =
  "increase" `assertEqual` ((), 94) $
  runState (increase 5) 89

testCountEmpty : IO ()
testCountEmpty =
  "countEmpty" `assertEqual` 7 $
  execState (countEmpty testTree) 0

testCountEmptyNode : IO ()
testCountEmptyNode =
  "countEmptyNode" `assertEqual` (7, 6) $
  execState (countEmptyNode testTree) (0, 0)

runTest : IO ()
runTest = do putStrLn "Testing Chapter 12: State"
             putStrLn infoLine
             runTests [ testIncrease
                      , testCountEmpty
                      , testCountEmptyNode
                      ]
