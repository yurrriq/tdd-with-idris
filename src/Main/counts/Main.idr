module Main

import Exercises.GettingStarted
-- import String.Extra

%access export

private
pluralize : String -> String -> Nat -> String
pluralize singular plural count =
  if count == 1
     then "1 " ++ singular
     else (show count) ++ " " ++ plural

private
showCounts : String -> String
showCounts str = let (wordCount, charCount) = counts str in
                     "\"" ++ str ++ "\" has " ++
                     pluralize "word" "words" wordCount ++ " in " ++
                     pluralize "character" "characters" charCount ++ ".\n"

||| A REPL that prompts for a string and prints its word and character counts.
partial
main : IO ()
main = repl "Enter a string: "
            showCounts
