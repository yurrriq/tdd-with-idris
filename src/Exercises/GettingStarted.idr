-- ------------------------------------------------------ [ GettingStarted.idr ]
-- Module      : Exercises.GettingStarted
-- Description : Solutions to the Chapter 2 exercises in Edwin Brady's
--               book, "Type-Driven Development with Idris."
-- --------------------------------------------------------------------- [ EOH ]
module Exercises.GettingStarted

%access export

-- ----------------------------------------------------------- [ 2.5 Exercises ]

exercise1_1 : (String, String, String)
exercise1_1 = ("A", "B", "C")

exercise1_2 : List String
exercise1_2 = ["A", "B", "C"]

exercise1_3 : ((Char, String), Char)
exercise1_3 = (('A', "B"), 'C')

namespace Simple

  ||| Determine whether a string reads the same backwards as forwards.
  ||| @ str A string to analyze.
  palindrome : (str : String) -> Bool
  palindrome str = str == reverse str

namespace CaseInsensitive

  ||| Determine, case insensitively, whether a string reads the same backwards
  ||| as forwards.
  ||| @ str A string to analyze.
  palindrome : (str : String) -> Bool
  palindrome = Simple.palindrome . toLower

namespace LongerThan10

  ||| Return `True` iff `str` is longer than 10 characts and case-insensitively
  ||| reads the same backwards as forwards.
  ||| @ str A string to analyze.
  caseInsensitivePalindrome' : (str : String) -> Bool
  caseInsensitivePalindrome' str =
    length str > 10 && CaseInsensitive.palindrome str

||| Compute the number of words and number of characters in a string.
||| @ str A string to analyze.
counts : (str : String) -> (Nat, Nat)
counts str =
  let wordCount = length (words str)
      charCount = length str
  in  (wordCount, charCount)

||| Return the largest ten values in a list.
||| @ list A list of values contstrained by `Ord`.
topTen : Ord a => (list : List a) -> List a
topTen = take 10 . sortBy (flip compare)

-- NOTE: This is a bit like Clojure's cond->.
syntax "cond$" [x] [b] [f] = if b then x else f x

||| Count the number of strings in list that are over a given length.
||| @ len the lower bound (exclusive) of string length.
overLength : (len : Nat) -> List String -> Nat
overLength len = foldr go 0
  where
    go : String -> Nat -> Nat
    go str n = cond$ n (length str > n) S

-- --------------------------------------------------------------------- [ EOF ]
