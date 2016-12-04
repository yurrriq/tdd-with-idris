-- ------------------------------------------------- [ InteractivePrograms.idr ]
-- Module      : Exercises.InteractivePrograms
-- Description : Solutions to the Chapter 5 exercises in Edwin Brady's
--               book, "Type-Driven Development with Idris."
-- --------------------------------------------------------------------- [ EOH ]
module Exercises.InteractivePrograms

import Control.Arrow
import Control.Category
import Data.Morphisms
import Data.Vect

import Effects
import Effect.Random

%access export

-- --------------------------------------------------------- [ 5.2.4 Exercises ]

||| Attempt to convert a string to a natural number.
|||
||| If `s` contains only digits, return `Just` the natural number it represents,
||| otherwise `Nothing`.
|||
||| ```idris example
||| stringToNat "123"
||| ```
||| ```idris example
||| stringToNat "bad"
||| ```
|||
||| @ string the string to try to convert
stringToNat : (string : String) -> Maybe Nat
stringToNat s = if all isDigit (unpack s)
                   then Just (cast s)
                   else Nothing

handleGuess : Nat -> IO () -> Maybe Nat -> IO ()
handleGuess _ loop Nothing           = putStrLn "Invalid number" *> loop
handleGuess target loop (Just guess) =
    case compare guess target of
         LT => putStrLn "Too low!"  *> loop
         EQ => putStrLn "Correct!"
         GT => putStrLn "Too high!" *> loop

readNumber : IO (Maybe Nat)
readNumber = stringToNat <$> getLine

namespace Simple

    ||| A simple "guess the number" game.
    |||
    ||| Repeatedly ask the user to guess a number and display whether the guess
    ||| is too high, too low, or correct. When the guess is correct, exit.
    |||
    ||| @ target the number to be guessed
    partial
    guess : (target : Nat) -> IO ()
    guess target = putStrLn "===> Guess the number" *> loop
      where
        partial
        loop : IO ()
        loop = putStr "> " *> handleGuess target loop !readNumber

    partial
    main : IO ()
    main = guess $ fromIntegerNat !(run (rndInt 1 100))

namespace Counting

    partial
    guess : (target, guesses : Nat) -> IO ()
    guess target guesses = putStrLn "===> Guess the number" *> loop
      where
        partial
        loop : IO ()
        loop = do putStrLn $ "Guesses so far: " ++ show guesses
                  putStr "> " *> handleGuess target loop !readNumber

    partial
    main : IO ()
    main = guess $ fromIntegerNat !(run (rndInt 1 100))

namespace DIY

    %hide Prelude.Interactive.replWith

    partial
    replWith : a -> String -> (a -> String -> Maybe (String, a)) -> IO ()
    replWith state prompt func =
        do putStr prompt
           case func state !getLine of
                Just (output, newState) => do putStr output
                                              replWith newState prompt func
                Nothing                 => pure ()

    %hide Prelude.Interactive.repl

    partial
    repl : String -> (String -> String) -> IO ()
    repl prompt func = replWith () prompt (flip (curry go))
      where
        go : (String, a) -> Maybe (String, a)
        go = applyMor $ first (arrow func) >>> arrow pure

-- --------------------------------------------------------- [ 5.3.5 Exercises ]

||| Read input from the console until the user enters a blank line.
partial
readToBlank : IO (List String)
readToBlank = case !getLine of
                   "" => pure []
                   x  => pure (x :: !readToBlank)

||| Read input from the console until the user enters a blank line,
||| then read a file name from the console, and write the input to the file.
partial
readAndSave : IO ()
readAndSave = do putStrLn "Input:"
                 contents <- unlines <$> readToBlank
                 putStr "Filename: "
                 Right file <- writeFile !getLine contents
                   | Left err => printLn err
                 putStrLn "done"

namespace ReadVectFile

    ||| Read the contents of a file into a dependent pair containing a length
    ||| and a `Vect` of that length.
    ||| If there are any errors, return the empty vector.
    partial
    readVectFile : (filename : String) -> IO (n ** Vect n String)
    readVectFile filename = do Right xs <- map lines <$> readFile filename
                                 | Left err => (printLn err *> pure (_ ** []))
                               pure (_ ** fromList xs)

    main : IO ()
    main = putStr "Filename: " *> printLn !(readVectFile !getLine)

-- --------------------------------------------------------------------- [ EOF ]
