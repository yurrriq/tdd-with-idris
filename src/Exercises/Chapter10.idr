-- ----------------------------------------------------------- [ Chapter10.idr ]
-- Module      : Exercises.Chapter10
-- Description : Solutions to the Chapter 10 exercises in Edwin Brady's
--               book, "Type-Driven Development with Idris."
-- --------------------------------------------------------------------- [ EOH ]
module Exercises.Chapter10

import Data.List.Views
import Data.Nat.Views
import Data.Vect
import Data.Vect.Views
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

---------------------------------------------------------- [ Exercise 10.2.5.1 ]

-- FIXME: I'm broken.
equalSuffix : Eq a => (xs, ys : List a) -> List a
equalSuffix = go []
  where
    go : List a -> List a -> List a -> List a
    go acc xs ys with (snocList xs)
      go acc [] _ | Empty = acc
      go acc (xs' ++ [x]) ys | (Snoc recX) with (snocList ys)
        go acc (_ ++ [_]) [] | (Snoc _) | Empty = acc
        go acc (xs' ++ [x]) (ys' ++ [y]) | (Snoc recX) | (Snoc recY) =
          let acc' = if x == y then x :: acc else acc in
          go acc' xs' ys' | recX | recY

---------------------------------------------------------- [ Exercise 10.2.5.2 ]

merge_sort : Ord a => Vect n a -> Vect n a
merge_sort xs with (splitRec xs)
  merge_sort []                | SplitRecNil = []
  merge_sort [x]               | SplitRecOne = [x]
  merge_sort (lefts ++ rights) | (SplitRecPair lrec rrec) =
    merge (merge_sort lefts  | lrec)
          (merge_sort rights | rrec)

---------------------------------------------------------- [ Exercise 10.2.5.3 ]

-- FIXME: This is ineffecient due to the string appending, at least.
toBinary : (n : Nat) -> String
toBinary n with (halfRec n)
  toBinary  Z          |  HalfRecZ         = ""
  toBinary (x + x)     | (HalfRecEven rec) = (toBinary x | rec) ++ "0"
  toBinary (S (x + x)) | (HalfRecOdd  rec) = (toBinary x | rec) ++ "1"

---------------------------------------------------------- [ Exercise 10.2.5.4 ]

palindrome : Eq a => (xs : List a) -> Bool
palindrome xs with (vList xs)
  palindrome []  | VNil = True
  palindrome [x] | VOne = True
  palindrome (x :: (ys ++ [y])) | (VCons rec) = x == y && palindrome ys | rec

---------------------------------------------------------------------- [ Tests ]

-- partial
testGroupByN : IO ()
testGroupByN = assertEq [[1,2,3],[4,5,6],[7,8,9],[10]] $
                        the (List (List Integer)) (groupByN 3 [1..10])

partial
testHalves10 : IO ()
testHalves10 = assertEq ([1..5], [6..10]) $ halves [1..10]

partial
testHalves1 : IO ()
testHalves1 = assertEq ([], [1]) $ the (List Integer, List Integer) (halves [1])

testEqualSuffix1 : IO ()
testEqualSuffix1 = assertEq (the (List Integer) [4,5])
                            (equalSuffix [1,2,4,5] [1..5])

testEqualSuffix2 : IO ()
testEqualSuffix2 = assertEq [] $ equalSuffix [1,2,4,5,6] [1..5]

testEqualSuffix3 : IO ()
testEqualSuffix3 = assertEq (the (List Integer) [4,5,6])
                            (equalSuffix [1,2,4,5,6] [1..6])

test_merge_sort1 : IO ()
test_merge_sort1 = assertEq (fromList [1,2,3]) (merge_sort [3,2,1])

test_merge_sort2 : IO ()
test_merge_sort2 = assertEq (fromList [1..9]) (merge_sort [5,1,4,3,2,6,8,7,9])

testToBinary42 : IO ()
testToBinary42 = assertEq "101010" $ toBinary 42

testToBinary94 : IO ()
testToBinary94 = assertEq "1011110" $ toBinary 94

testToBinaryExercism : IO ()
testToBinaryExercism =
  assertEq (the (List String) ["1", "10", "11", "100", "1001", "11010"])
           (map toBinary [1,2,3,4,9,26])

testPalindrome1 : IO ()
testPalindrome1 = assertEq True $ palindrome (unpack "abccba")

testPalindrome2 : IO ()
testPalindrome2 = assertEq True $ palindrome (unpack "abcba")

testPalindrome3 : IO ()
testPalindrome3 = assertEq False $ palindrome (unpack "abcb")

------------------------------------------------------------------------ [ EOF ]
