= Chapter 2 Exercises

> module Chapter2
>
> import Data.Vect
>
> %access public export
> %default total


== 1

```idris
-- 1
("A", "B", "C") : (String, String, String)

-- 2
["A", "B", "C"] : List String -- or
["A", "B", "C"] : Vect 3 String

-- 3
(('A', "B"), 'C') : ((Char, String), Char)
```


== 2

```idris
palindrome : String -> Bool
```

`M-RET d`

```idris
palindrome : String -> Bool
palindrome x = ?palindrome_rhs
```

Rename `x` to `word` and fill in the right hand side.


> ||| Determine whether a string reads the same backwards as forwards.
> ||| @ str a string analyze.
> palindrome : (str : String) -> Bool
> palindrome str = str == reverse str


== 3

> ||| Like `palindrome`, but case insensitive.
> ||| @ str a string analyze.
> caseInsensitivePalindrome : (str : String) -> Bool
> caseInsensitivePalindrome = palindrome . toLower


== 4

> ||| Like `caseInsensitivePalindrome`, but return `True`
> ||| iff `str` is longer than 10 characters.
> ||| @ str a string analyze.
> caseInsensitivePalindrome' : (str : String) -> Bool
> caseInsensitivePalindrome' str =
>   length str > 10 && caseInsensitivePalindrome str


== 5


> ||| Like `caseInsensitivePalindrome'`, but with the lower bound parameterized.
> |||
> ||| `caseInsensitivePalindrome'` can be thought to be defined as
> ||| `ciPalindromeWithLowerBound 10`
> ||| @ lb the lower bound (exclusive) for string length.
> ||| @ str a string analyze.
> ciPalindromeWithLowerBound : (lb : Nat) -> (str : String) -> Bool
> ciPalindromeWithLowerBound lb str =
>   length str > lb && caseInsensitivePalindrome str


== 6

> ||| Compute the number of words and number of characters in a string.
> ||| @ str a string to analyze.
> counts : (str : String) -> (Nat, Nat)
> counts str =
>   let wordCount = length (words str)
>       charCount = length str
>   in  (wordCount, charCount)

> ||| `"Hello, Idris world!"` has three words and 19 characters.
> testCounts : counts "Hello, Idris world!" = (3, 19)
> testCounts = Refl


== 7

> ||| Return the largest ten values in a list.
> ||| @ list a list of values contstrained by `Ord`.
> top_ten : Ord a => (list : List a) -> List a
> top_ten = take 10 . sortBy (flip compare)


== 8

> ||| Count the number of strings in list that are over a given length.
> ||| @ len the lower bound (exclusive) of string length.
> over_length : (len : Nat) -> List String -> Nat
> over_length len = foldr go 0
>   where go : String -> Nat -> Nat
>         go x count = if length x > len then S count else count

> test_over_length : over_length 3 ["One", "Two", "Three", "Four"] = 2
> test_over_length = Refl


== 9

See [`Chapter2Main`](Chapter2Main.md).
