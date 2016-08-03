module Test

%access  public export
%default total

passFail        : (b : Bool) -> IO ()
passFail b      = putStrLn $ ifThenElse b "Passed" "Failed"

assertEq        : Eq a => (given : a) -> (expected : a) -> IO ()
assertEq g e    = putStr "Test " *> passFail (g == e)

assertNotEq     : Eq a => (given : a) -> (expected : a) -> IO ()
assertNotEq g e = putStr "Test " *> passFail (g == e)
