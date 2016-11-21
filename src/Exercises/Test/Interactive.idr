-- --------------------------------------------------------- [ Interactive.idr ]
-- Module      : Exercises.Test.Interactive
-- Description : Test covering the Chapter 3 exercises in Edwin Brady's
--               book, "Type-Driven Development with Idris."
-- --------------------------------------------------------------------- [ EOH ]
module Exercises.Test.Interactive

import Exercises.Interactive
import Test.Helpers

%access export

testLength : IO ()
testLength = "my_length" `assertEqual` 10 $ my_length [1..10]

testReverse : IO ()
testReverse = "my_reverse" `assertEqual` [10,9..1] $ my_reverse [1..10]

testMap : IO ()
testMap = "my_map" `assertEqual` [2,4..20] $ my_map (*2) [1..10]

testVectMap : IO ()
testVectMap =
  "my_vect_map" `assertEqual` the (Vect _ Nat) [3, 3, 7, 4] $
  my_vect_map length ["Hot", "Dog", "Jumping", "Frog"]

testAddMatrix : (Eq elem, Num elem, Show elem) =>
                (a, b, expected : Vect n (Vect m elem)) ->
                IO ()
testAddMatrix {n} {m} a b expected =
  unwords ["addMatrix", show n, "x", show m] `assertEqual` expected $
  addMatrix a b

testMultMatrix : (Eq elem, Num elem, Show elem) =>
                 Vect n (Vect m elem) -> Vect m (Vect p elem) ->
                 Vect n (Vect p elem) ->
                 IO ()
testMultMatrix {n} {m} {p} a b expected =
  unwords ["multMatrix (", show n, "x", show m, ") (", show m, "x", show p, ")"]
  `assertEqual` expected $ multMatrix a b

testTransposeMat : (Eq elem, Show elem) =>
                   Vect n (Vect m elem) -> Vect m (Vect n elem) ->
                   IO ()
testTransposeMat {n} {m} toTranspose expected =
  unwords ["transpose_mat", show n, "x", show m] `assertEqual` expected $
  transpose_mat toTranspose

runTest : IO ()
runTest = do putStrLn "Testing Chapter 3: Interactive development with types"
             putStrLn infoLine
             runTests [ testLength
                      , testReverse
                      , testMap
                      , testVectMap
                      , testTransposeMat [ [1, 2,   3,  4],
                                           [5, 6,   7,  8],
                                           [9, 10, 11, 12] ]
                                         [ [1, 5,  9],
                                           [2, 6, 10],
                                           [3, 7, 11],
                                           [4, 8, 12] ]
                      , testTransposeMat [ [1, 2],
                                           [3, 4],
                                           [5, 6] ]
                                         [ [1, 3, 5],
                                           [2, 4, 6] ]
                      , testAddMatrix [ [1, 2],
                                        [3, 4] ]
                                      [ [5, 6],
                                        [7, 8] ]
                                      [ [ 6,  8],
                                        [10, 12] ]
                      , testMultMatrix [ [  1,   2],
                                         [  3,   4],
                                         [  5,   6] ]
                                       [ [  7,   8,   9,  10],
                                         [ 11,  12,  13,  14] ]
                                       [ [ 29,  32,  35,  38],
                                         [ 65,  72,  79,  86],
                                         [101, 112, 123, 134] ]                                       
                      ]

-- --------------------------------------------------------------------- [ EOF ]
