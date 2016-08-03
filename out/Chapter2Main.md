```idris
module Chapter2Main

import Exercises.Chapter2

%access public export

infixl 9 >>> -- like Idris's (.)
||| Left-to-right composition.
||| @f a function to apply first
||| @g a function to apply to the result of `f x`.
(>>>) : (f : a -> b) -> (g : b -> c) -> (a -> c)
(>>>) f g = g . f


namespace Palindrome
  showPalindrome : String -> String
  showPalindrome word =
    let result = if palindrome word then "is" else "is not"
    in  word ++ " " ++ result ++ " a palindrome.\n"

  main : IO ()
  main = repl "Enter a potential palindrome: "
              showPalindrome


namespace Counts
  showCounts : String -> String
  showCounts = (show . counts) >>> (++ "\n")

  main : IO ()
  main = repl "Enter a string: "
              showCounts
```
