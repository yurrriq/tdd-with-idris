-- ----------------------------------------------------------- [ Chapter10.idr ]
-- Module      : Exercises.Chapter10
-- Description : Solutions to the Chapter 10 exercises in Edwin Brady's
--               book, "Type-Driven Development with Idris."
-- --------------------------------------------------------------------- [ EOH ]
module Exercises.Chapter10

import Test

%access  export
%default total

---------------------------------------------------------- [ Exercise 10.1.6.1 ]

public export
data TakeN : List a -> Type where
     Fewer : TakeN xs
     Exact : (n_xs : List a) -> TakeN (n_xs ++ rest)

takeN : (n : Nat) -> (xs : List a) -> TakeN xs
takeN _ [] = Fewer
takeN Z _  = Exact []
takeN (S Z) (x :: xs) = Exact [x]
takeN (S k) (x :: xs) with (takeN k xs)
  takeN (S k) (x :: xs) | Fewer = Fewer
  takeN (S k) (x :: (n_xs ++ rest)) | (Exact n_xs) = Exact (x :: n_xs)

-- partial
groupByN : (n : Nat) -> (xs : List a) -> List (List a)
groupByN Z _ = [[]]           -- Provide a sensible base case.
groupByN n xs with (takeN n xs)
 groupByN n  xs            | Fewer      = [xs]
 -- HACK: Assert to the totality checker that groupByN will always terminate.
 groupByN n (n_xs ++ rest) | Exact n_xs = n_xs :: assert_total (groupByN n rest)
 -- groupByN n (n_xs ++ rest) | Exact n_xs = n_xs :: groupByN n rest

---------------------------------------------------------- [ Exercise 10.1.6.2 ]

partial
halves : List a -> (List a, List a)
halves xs with (takeN (length xs `div` 2) xs)
  halves xs | Fewer = ([],xs)
  halves (n_xs ++ rest) | (Exact n_xs) {rest} = (n_xs,rest)

---------------------------------------------------------------------- [ Tests ]

-- partial
testGroupByN : IO ()
testGroupByN = assertEq [[1,2,3],[4,5,6],[7,8,9],[10]] $ groupByN 3 [1..10]

partial
testHalves10 : IO ()
testHalves10 = assertEq ([1..5], [6..10]) $ halves [1..10]

partial
testHalves1 : IO ()
testHalves1 = assertEq ([], [1]) $ halves [1]

------------------------------------------------------------------------ [ EOF ]
