module Main


import Control.Arrow
import Control.Category
import Data.Morphisms
import System

%access public export

readNumber : IO (Maybe Nat)
readNumber = do
  input <- getLine
  if   all isDigit (unpack input)
  then pure (Just (cast input))
  else pure Nothing


-- Exercise 5.2.4.{1,3}

partial
guess : (target, guesses : Nat) -> IO ()
guess target guesses = do
  let guesses' = S guesses
  putStr "Guess #"
  print guesses'
  putStr " "
  Just num <- readNumber
    | Nothing => do putStrLn "Invalid input"
                    guess target guesses'
  EQ <- pure $ compare num target
    | LT => do putStrLn "Too low"
               guess target guesses'
    | GT => do putStrLn "Too high"
               guess target guesses'
  putStrLn "Nailed it!"


-- Exercise 5.2.4.2

main : IO ()
main = time >>= pure . flip mod 100 >>= flip guess 0 . cast


-- Exercise 5.2.4.4

{-
syntax "(,)" = MkPair

infixl 8 .:

(.:) : (c -> d) -> (a -> b -> c) -> a -> b -> d
(.:) = (.) . (.)
-}

namespace DIY
  replWith : a -> String -> (a -> String -> Maybe (String, a)) -> IO ()
  replWith state prompt func = do
    putStr prompt
    input <- getLine
    case func state input of
      Just (output, state') => putStr output *> DIY.replWith state' prompt func
      Nothing               => pure ()

  repl : String -> (String -> String) -> IO ()
  repl prompt func = DIY.replWith () prompt (go func)
    where
      go : (a -> b) -> c -> a -> Maybe (b,c)
      go f = curry . applyMor $
        arrow swap >>> first (arrow f) >>> arrow Just
