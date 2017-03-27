module Main

import Control.IOExcept
import Data.Vect

import Effects
import Effect.File
import Effect.Random
import Effect.StdIO
import Effect.System

%access public export

data HState      = NotRunning
                 | Running Nat Nat

data GuessResult = Correct | Incorrect

data Hangman     : HState -> Type where
     GameStart   : Hangman NotRunning
     GameWon     : (word    : String)                -> Hangman NotRunning
     GameLost    : (word    : String)                -> Hangman NotRunning
     InProgress  : (word    : String)                ->
                   (guesses : Nat)                   ->
                   (got     : List Char)             ->
                   (missing : Vect letters Char)     ->
                   Hangman (Running guesses letters)

implementation Default (Hangman NotRunning) where
  default = GameStart

implementation Show (Hangman state) where
  show GameStart                             = "Starting"
  show (GameWon word)                        = "Game won: word was " ++ word
  show (GameLost word)                       = "Game lost: word was " ++ word
  show (InProgress word guesses got missing) =
      unlines [ pack (map hideMissing (unpack word))
              , "Got " ++ show got ++ " so far"
              , show guesses ++ " guesses left"
              ]
    where
      hideMissing : Char -> Char
      hideMissing c = ifThenElse (c `elem` missing) '_' c

export
letters : String -> List Char
letters = map toUpper . nub . unpack

%access private

initState : (word : String) -> Hangman (Running 6 (length (letters word)))
initState word = InProgress (toUpper word) _ [] (fromList (letters word))

removeElem : (x : a) -> (xs : Vect (S n) a) ->
             {auto prf : Elem x xs} ->
             Vect n a
removeElem x (x :: ys) {prf = Here}                  = ys
removeElem {n = Z} x [y] {prf = There later}         = absurd later
removeElem {n = S _} x (y :: ys) {prf = There later} = y :: removeElem x ys

%access public export

data HangmanRules : Effect where
     NewGame      : (word : String) ->
                    sig HangmanRules ()
                        (Hangman NotRunning)
                        (Hangman (Running 6 (length (letters word))))
     ||| No guesses left, so the player has lost.
     Lost         : sig HangmanRules ()
                        (Hangman (Running 0 (S letters)))
                        (Hangman NotRunning)
     ||| No letters left to guess, so the player has won.
     Won          : sig HangmanRules ()
                        (Hangman (Running (S guesses) 0))
                        (Hangman NotRunning)
     Guess        : (letter : Char) ->
                    sig HangmanRules GuessResult
                        (Hangman (Running (S guesses) (S letters)))
                        (\res =>
                             Hangman $
                             case res of
                                  Correct   => Running (S guesses) letters
                                  Incorrect => Running guesses (S letters))
     ShowState    : sig HangmanRules String (Hangman state)
     -- Message      : (message : String) -> sig HangmanRules () (Hangman state)
     -- ReadGuess    : sig HangmanRules Char (Hangman state)

HANGMAN : HState -> EFFECT
HANGMAN state = MkEff (Hangman state) HangmanRules

%access export

guess : (letter : Char) ->
        Eff GuessResult
            [HANGMAN (Running (S guesses) (S letters))]
            (\res =>
                [HANGMAN (case res of
                               Correct   => Running (S guesses) letters
                               Incorrect => Running guesses (S letters))])
guess letter = call (Guess letter)

won : Eff () [HANGMAN (Running (S guesses) 0)] [HANGMAN NotRunning]
won = call Won

lost : Eff () [HANGMAN (Running 0 (S letters))] [HANGMAN NotRunning]
lost = call Lost

newGame : (word : String) ->
          Eff () [HANGMAN NotRunning]
                 [HANGMAN (Running 6 (length (letters word)))]
newGame word = call (NewGame word)

showState : Eff String [HANGMAN state]
showState = call ShowState

public export
implementation Handler HangmanRules m where
  handle (InProgress word _ _ _) Won k  = k () (GameWon word)
  handle (InProgress word _ _ _) Lost k = k () (GameLost word)
  handle state ShowState  k             = k (show state) state
  handle _ (NewGame word) k             = k () (initState word)
  handle (InProgress word (S guesses) got missing) (Guess letter) k =
      case isElem letter missing of
           Yes _ => k Correct $ InProgress word _ (letter :: got)
                                           (removeElem letter missing)
           No _  => k Incorrect (InProgress word _ got missing)

partial
game : Eff () [HANGMAN (Running (S guesses) letters), STDIO]
              [HANGMAN NotRunning, STDIO]
game {letters = Z}             = won
game {guesses} {letters = S k} =
    do putStrLn !showState
       putStr "Enter guess: "
       case !(unpack . trim <$> getStr) of
            [letter] => case !(guess (toUpper letter)) of
                             Correct   => do putStrLn "Correct"
                                             case k of
                                               Z   => won
                                               S j => game
                             Incorrect => do putStrLn "Incorrect"
                                             case guesses of
                                                  Z   => lost
                                                  S k => game
            _        => do putStrLn "Invalid input!"
                           game

words : ?wlen
words = with Vect [ "idris", "agda", "haskell", "miranda",
                    "java", "javascript", "fortran", "basic", "racket",
                    "coffeescript", "rust", "purescript", "clean", "links",
                    "koka", "cobol" ]

wlen = %runElab search

%default partial

runGame : Eff () [HANGMAN NotRunning, FILE (), RND, STDIO, SYSTEM]
runGame = do srand !time
             newGame !(rndSelect' words)
             game
             putStrLn !showState

main : IO ()
main = run runGame

-- --------------------------------------------------------------------- [ EOF ]
