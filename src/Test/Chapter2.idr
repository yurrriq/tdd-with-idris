module Test.Chapter2

import Test
import Exercises.Chapter2

%access  public export
%default total

||| `"Hello, Idris world!"` has three words and 19 characters.
testCounts : IO ()
testCounts = assertEq (3, 19) (counts "Hello, Idris world!")

||| `["One", "Two", "Three", "Four"]` has two strings over three characters.
test_over_length : IO ()
test_over_length = assertEq 2 (over_length 3 ["One", "Two", "Three", "Four"])
