-- ------------------------------------------------------------- [ Helpers.idr ]
-- Module      : Test.Helpers
-- Description : Helper functions for writing tests.
-- --------------------------------------------------------------------- [ EOH ]
module Test.Helpers

import public Test.Generic

import Data.Vect

%access export

||| Like Test.Assertions.assertEqual but with `given` and `expected` swapped.
assertEqual' : (Eq a, Show a) =>
               (title : String) ->
               (expected, given : a) ->
               IO Bool
assertEqual' title expected given = genericTest (Just title) given expected (==)

namespace SingleResult
  summaryLine : Bool -> String
  summaryLine True  = succLine
  summaryLine False = errLine

namespace ManyResults
  summaryLine : Foldable t => t Bool -> String
  summaryLine = summaryLine . all id

printSummary : Vect n Bool -> IO ()
printSummary = putStrLn . unlines . ([summaryLine, summary] <*>) . pure

testChapter : (label : String) ->
              (tests : List (IO Bool)) ->
              IO (Vect (length tests) Bool)
testChapter label tests =
    do putStrLn $ "Testing Chapter " ++ label
       putStrLn infoLine
       results <- sequence (fromList tests)
       printSummary results
       pure results

-- --------------------------------------------------------------------- [ EOF ]
