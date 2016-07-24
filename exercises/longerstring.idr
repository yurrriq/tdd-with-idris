module Main

%access  public export
%default total

||| Given two strings, return the length of the longer one.
longerLength : String -> String -> Nat
longerLength = max `on` length
  where
    on : (b -> b -> c) -> (a -> b) -> a -> a -> c
    on g f x y = f x `g` f y


-- Exercise 5.1.4.1

namespace Do
  main : IO ()
  main = do
    putStr "String A: "
    stringA <- getLine
    putStr "String B: "
    stringB <- getLine
    printLn $ longerLength stringA stringB


-- Exercise 5.1.4.2

namespace Bind
  main : IO ()
  main =
    putStr "String A: " *> getLine >>= \stringA =>
    putStr "String B: " *> getLine >>= \stringB =>
    printLn $ longerLength stringA stringB
