module Main

import Exercises.GettingStarted

%access export

private
boolToString : Bool -> String
boolToString True  = "is"
boolToString False = "is not"

private
showPalindrome : String -> String
showPalindrome str =
  let mightBe = boolToString (CaseInsensitive.palindrome str) in
      unwords [ str, mightBe, "a palindrome.\n" ]

partial
main : IO ()
main = repl "Enter a potential palindrome: "
            showPalindrome
