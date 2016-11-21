-- ------------------------------------------------------ [ GettingStarted.idr ]
-- Module      : Exercises.Test.GettingStarted
-- Description : Test covering the Chapter 2 exercises in Edwin Brady's
--               book, "Type-Driven Development with Idris."
-- --------------------------------------------------------------------- [ EOH ]
module Exercises.Test.GettingStarted

import Exercises.GettingStarted
import Test.Helpers

%access export

private
testPalindrome' : String -> (String -> Bool) -> String -> Bool -> IO ()
testPalindrome' label palindrome string isPalindrome =
  unwords [label, show string] `assertEqual` isPalindrome $
  palindrome string

testPalindrome : String -> Bool -> IO ()
testPalindrome = testPalindrome' "Simple.palindrome" Simple.palindrome

testPalindromeCI : String -> Bool -> IO ()
testPalindromeCI =
  testPalindrome' "CaseInsensitive.palindrome" CaseInsensitive.palindrome

testLongPalindrome : String -> Bool -> IO ()
testLongPalindrome =
  testPalindrome' "LongerThan10.palindrome" LongerThan10.palindrome

testPalindromeLen : Nat -> String -> Bool -> IO ()
testPalindromeLen =
  testPalindrome' "LongerThanN.palindrome" . LongerThanN.palindrome

||| `"Hello, Idris world!"` has three words and 19 characters.
testCounts : IO ()
testCounts =
  "counts" `assertEqual` (3, 19) $
  counts "Hello, Idris world!"

||| `["One", "Two", "Three", "Four"]` has two strings over three characters.
testOverLength : IO ()
testOverLength =
  "overLength" `assertEqual` 2 $
  overLength 3 ["One", "Two", "Three", "Four"]

testTopTen : IO ()
testTopTen =
  "topTen" `assertEqual` [100,99..91] $
  topTen [1..100]

runTest : IO ()
runTest = do putStrLn "Testing Chapter 2: Getting Started"
             putStrLn infoLine
             runTests [ testPalindrome "racecar" True
                      , testPalindrome "race car" False
                      , testPalindromeCI "Racecar" True
                      , testLongPalindrome "racecar" False
                      , testLongPalindrome "able was i ere i saw elba" True
                      , testPalindromeLen 10 "racecar" False
                      , testPalindromeLen 5 "racecar" True
                      , testCounts
                      , testTopTen
                      , testOverLength
                      ]

-- --------------------------------------------------------------------- [ EOF ]
