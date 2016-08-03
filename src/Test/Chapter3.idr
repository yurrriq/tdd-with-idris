module Test.Chapter3

import Data.Vect
import Test
import Exercises.Chapter3

%access public export

testAddMatrix : IO ()
testAddMatrix =
  assertEq  [ [ 8, 10]
            , [12, 14]
            , [16, 18] ]
           ([ [ 1,  2]
            , [ 3,  4]
            , [ 5,  6] ]
            `addMatrix`
            [ [ 7,  8]
            , [ 9, 10]
            , [11, 12] ])
