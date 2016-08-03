module Test.Chapter4

import Data.Vect
import Test
import Exercises.Chapter4

%access public export

testOneTwo : IO ()
testOneTwo = assertEq [1..9] (treeToList (listToTree [4,1,8,7,2,3,9,5,6]))

testMaxMaybe1 : IO ()
testMaxMaybe1 = assertEq (Just 5) (maxMaybe (Just 4) (Just 5))

testMaxMaybe2 : IO ()
testMaxMaybe2 = assertEq (Just 4) (maxMaybe (Just 4) Nothing)

testMaxMaybe3 : IO ()
testMaxMaybe3 = assertEq (Just 5) (maxMaybe Nothing (Just 5))


rectangle : Picture
rectangle = Primitive (Rectangle 20 10)

circle : Picture
circle = Primitive (Circle 5)

triangle : Picture
triangle = Primitive (Triangle 10 10)

test_picture : Picture
test_picture = Combine (Translate 5 5 rectangle)
                       (Combine (Translate 35 5 circle)
                                (Translate 15 25 triangle))

testBiggestTriangle1 : IO ()
testBiggestTriangle1 =
  assertEq (Just (picture_area triangle)) (biggestTriangle test_picture)


testSumEntries1 : IO ()
testSumEntries1 = assertEq (Just 10) (sumEntries 2 [1,2,3,4] [5,6,7,8])

testSumEntries2 : IO ()
testSumEntries2 = assertEq Nothing (sumEntries 4 [1,2,3,4] [5,6,7,8])
