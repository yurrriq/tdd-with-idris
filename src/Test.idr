||| Helper functions for writing simple tests.
module Test

%access  export
%default total

||| If given boolean is `True`, print Passed, otherwise print Failed.
||| @ b A boolean on which to perform case analysis.
passFail        : (b : Bool) -> IO ()
passFail b      = putStrLn $ if b then "Passed" else "Failed"

||| Determine whether `given` is equal to `expected`
||| and call `passFail` on the result.
||| @ given    A value to compare, i.e. the result of a function to test.
||| @ expected The expected value.
assertEq        : Eq a => (given, expected : a) -> IO ()
assertEq g e    = putStr "Test " *> passFail (g == e)

||| Determine whether `given` is not equal to `expected`
||| and call `passFail` on the result.
||| @ given    A value to compare, i.e. the result of a function to test.
||| @ expected The expected value.
assertNotEq     : Eq a => (given, expected : a) -> IO ()
assertNotEq g e = putStr "Test " *> passFail (g /= e)
