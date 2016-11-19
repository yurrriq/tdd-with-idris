module Test.Helpers

import public Test.Generic

%default total
%access export

assertEqual : (Eq a, Show a) =>
              (title : String) ->
              (expected, given : a) ->
              IO ()
assertEqual title expected given =
  genericTest (Just title) given expected (==)
