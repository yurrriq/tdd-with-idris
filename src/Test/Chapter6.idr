module Test.Chapter6

import Test
import Exercises.Chapter6

%access public export

-- FIXME: This is a little weird...
testTupleVect : IO ()
testTupleVect = assertEq (1,2,3,4,()) (the (TupleVect 4 Nat) (1,2,3,4,()))
