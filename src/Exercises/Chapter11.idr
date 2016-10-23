-- ----------------------------------------------------------- [ Chapter11.idr ]
-- Module      : Exercises.Chapter11
-- Description : Solutions to the Chapter 11 exercises in Edwin Brady's
--               book, "Type-Driven Development with Idris."
-- --------------------------------------------------------------------- [ EOH ]
module Exercises.Chapter11

import Data.Primitives.Views
import Test

%access  export
%default total

-- ------------------------------------------------------- [ Exercise 11.1.7.1 ]

everyOther : Stream a -> Stream a
everyOther (_ :: x :: xs) = x :: everyOther xs

-- ------------------------------------------------------ [ Exercises 11.1.7.2 ]

||| Generic infinite lists
data InfList : (elem : Type) -> Type where
  ||| An infinite list, consisting of a head element and an infinite list.
  (::) : (x : elem) -> (xs : Inf (InfList elem)) -> InfList elem

%name InfList xs, ys, zs

countFrom : Integer -> InfList Integer
countFrom x = x :: countFrom (x + 1)

getPrefix : (count : Nat) -> InfList a -> List a
getPrefix  Z    _  = []
getPrefix (S k) (x :: xs) = x :: getPrefix k xs

implementation Functor InfList where
  map f (x :: xs) = f x :: map f xs -- FIXME: Delay is implicit, right?

-- ------------------------------------------------------- [ Exercise 11.1.7.3 ]

randoms : Int -> Stream Int
randoms seed = let seed' = 1664525 * seed + 1013904223 in
                  (seed' `shiftR` 2) :: randoms seed'

{-
mutual
  even : Nat -> Bool
  even  Z    = True
  even (S k) = odd k

  odd : Nat -> Bool
  odd  Z    = False
  odd (S k) = even k
-}

||| The faces of a coin, heads or tails.
data Face = Heads | Tails

implementation Eq Face where
  (==) Heads Heads = True
  (==) Tails Tails = True
  (==) _     _     = False

syntax [test] "?" [t] ":" [e] = if test then t else e;

private
getFace : Int -> Face
getFace x with (divides x 2)
  getFace (_ + rem) | (DivBy _) = (rem == 1) ? Tails : Heads

coinFlips : (count : Nat) -> Stream Int -> List Face
coinFlips  Z     _        = []
coinFlips (S k) (x :: xs) = getFace x :: coinFlips k xs

-- ------------------------------------------------------- [ Exercise 11.1.7.4 ]

private
squareRootApprox : (number, approx : Double) -> Stream Double
squareRootApprox number approx =
  let next = (approx + (number / approx)) / 2
  in  approx :: squareRootApprox number next

-- ------------------------------------------------------- [ Exercise 11.1.7.5 ]

squareRootBound : (max : Nat) -> (number, bound : Double)
               -> (approxs : Stream Double)
               -> Double
squareRootBound  Z    _      _     (x :: _)  = x
squareRootBound (S k) number bound (x :: xs) =
  if bound > abs (x * x)
    then x
    else squareRootBound k number bound xs

squareRoot : (number : Double) -> Double
squareRoot number = squareRootBound 100 number 0.00000000001
                      (squareRootApprox number number)

-- ------------------------------------------------------------------- [ InfIO ]

namespace InfIO
  data InfIO : Type where
       Do    : IO a -> (a -> Inf InfIO) -> InfIO

  (>>=) : IO a -> (a -> Inf InfIO) -> InfIO
  (>>=) = Do

data Fuel = Dry | More (Lazy Fuel)

tank : Nat -> Fuel
tank  Z    = Dry
tank (S k) = More (tank k)

partial
forever : Fuel
forever = More forever

run : Fuel -> InfIO -> IO ()
run  Dry         _       = putStrLn "Out of fuel"
run (More fuel) (Do c f) = do res <- c
                              run fuel (f res)

-- --------------------------------------------------------- [ Exercise 11.2.7 ]

totalREPL : (prompt : String) -> (action : String -> String) -> InfIO
totalREPL prompt action
  = do putStr prompt
       input <- getLine
       putStr (action input)
       totalREPL prompt action

-- ------------------------------------------ [ 11.3.2 Domain Specific Comands ]

data Command  : Type -> Type where
     PutStr   : String -> Command ()
     PutStrLn : String -> Command ()
     GetLine  : Command String

     Pure     : ty -> Command ty
     Bind     : Command a -> (a -> Command b) -> Command b

-- ------------------------------------------------------- [ Exercise 11.3.4.2 ]
     ReadFile  : (filepath : String) -> Command (Either FileError String)
     WriteFile : (filepath, contents : String) -> Command (Either FileError ())

namespace CommandDo
  (>>=) : Command a -> (a -> Command b) -> Command b
  (>>=) = Bind

runCommand : Command a -> IO a
runCommand (PutStr   str) = putStr str
runCommand (PutStrLn str) = putStrLn str
runCommand  GetLine       = getLine
runCommand (Pure val)     = pure val
runCommand (Bind c f)     = runCommand c >>= runCommand . f
runCommand (ReadFile  filepath)          = readFile filepath
runCommand (WriteFile filepath contents) = writeFile filepath contents

data ConsoleIO : Type -> Type where
     Quit      : a -> ConsoleIO a
     Do        : Command a -> (a -> Inf (ConsoleIO b)) -> ConsoleIO b

namespace ConsoleDo
  (>>=) : Command a -> (a -> Inf (ConsoleIO b)) -> ConsoleIO b
  (>>=) = Do

  run : Fuel -> ConsoleIO a -> IO (Maybe a)
  run       fuel  (Quit val) = do pure (Just val)
  run (More fuel) (Do c f)   = do res <- runCommand c
                                  run fuel (f res)
  run  Dry         p         = pure Nothing

  run_ : Fuel -> ConsoleIO a -> IO ()
  run_       fuel  (Quit val) = pure ()
  run_ (More fuel) (Do c f)   = do res <- runCommand c
                                   run_ fuel (f res)
  run_  Dry         p         = pure ()

  partial
  runForever : ConsoleIO a -> IO (Maybe a)
  runForever = run forever

  partial
  runForever_ : ConsoleIO a -> IO ()
  runForever_ = run_ forever

-- ---------------------------------------------- [ 11.3.3 Sequencing Commands ]

data Input = Answer Int
           | QuitCmd

readInput : (prompt : String) -> Command Input
readInput prompt = do PutStr prompt
                      answer <- GetLine
                      if toLower answer == "quit"
                         then Pure QuitCmd
                         else Pure (Answer (cast answer))

-- ------------------------------------------------------- [ Exercise 11.3.4.1 ]

private
arithInputs : (seed : Int) -> Stream Int
arithInputs seed = map bound (randoms seed)
  where
    bound : Int -> Int
    bound num with (divides num 12)
      bound ((12 * div) + rem) | (DivBy prf) = abs rem + 1

mutual
  correct : Stream Int -> (score, attempts : Nat) -> ConsoleIO Double
  correct nums score attempts
    = do PutStrLn "Correct!"
         quiz nums (score + 1) attempts

  wrong : Stream Int -> Int -> (score, attempts : Nat) -> ConsoleIO Double
  wrong nums answer score attempts
    = do PutStrLn $ "Wrong, the answer is: " ++ show answer
         quiz nums score attempts

-- ------------------------------------------------------- [ Exercise 11.3.4.1 ]

  quiz : Stream Int -> (score, attempts : Nat) -> ConsoleIO Double
  quiz (num1 :: num2 :: nums) score attempts
    = do PutStrLn $ "Score so far: " ++ show score ++ " / " ++ show attempts
         input <- readInput $ show num1 ++ " * " ++ show num2 ++ "? "
         case input of
              Answer answer =>
                let attempts'     = attempts + 1
                    correctAnswer = num1 * num2 in
                if answer == correctAnswer
                   then correct nums score attempts'
                   else wrong nums correctAnswer score attempts'
              QuitCmd =>
                Quit (100 * cast score / cast attempts)

-- ------------------------------------------------------- [ Exercise 11.3.4.3 ]

mutual
  ||| Read the contents of a file and print it to stdout with a trailing
  ||| newline. Print an error message if filename is not a normal file.
  cat : (filename : String) -> ConsoleIO ()
  cat filename
    = do Right contents <- ReadFile filename
           | Left err => do PutStrLn (show err)
                            shell
         PutStrLn contents
         shell

  ||| Read the contents of a source file and write it to a destination file.
  ||| Print an error message if anything goes wrong.
  ||| @ source a source filepath, must be a normal file
  ||| @ destination a destination filepath, must be writable
  copy : (source, destination : String) -> ConsoleIO ()
  copy source destination
    = do Right contents <- ReadFile source
           | Left err => do PutStrLn (show err)
                            shell
         Right () <- WriteFile destination contents
           | Left err => do PutStrLn (show err)
                            shell
         shell

  ||| Print a warning about an invalid command.
  ||| @ command a string that represents an invalid command
  invalidCommand : (command : String) -> ConsoleIO ()
  invalidCommand command
    = do PutStrLn ("Invalid command: " ++ command)
         shell

  ||| Quit the shell.
  exit : ConsoleIO ()
  exit = Quit ()

  ||| Run a limited, interactive shell.
  ||| Supported commands:
  |||   - cat [filename]
  |||   - copy [source] [destination]
  |||   - exit
  shell : ConsoleIO ()
  shell = do PutStr "> "
             input <- GetLine
             case words input of
                  ["exit"] => exit
                  ["cat", filepath] => cat filepath
                  ["copy", source, destination] => copy source destination
                  _ => invalidCommand input

-- ------------------------------------------------------------------- [ Tests ]

testEveryOther : IO ()
testEveryOther = assertEq [2,4..20] $ take 10 (everyOther [1..])

testInfListFunctor : IO ()
testInfListFunctor = assertEq [2,4..20] $ getPrefix 10 (map (*2) (countFrom 1))

testCoinFlips : IO ()
testCoinFlips = assertEq [Tails,Heads,Tails,Tails,Heads,Tails] $
                         coinFlips 6 (randoms 12345)

testSquareRootApprox1 : IO ()
testSquareRootApprox1 = assertEq [10.0,5.5,3.659090909090909] $
                                 take 3 (squareRootApprox 10 10)

testSquareRootApprox2 : IO ()
testSquareRootApprox2 = assertEq [25.0,14.5,10.698275862068964] $
                                 take 3 (squareRootApprox 100 25)

testSquareRoot1 : IO ()
testSquareRoot1 = assertEq 2.449489742783178 (squareRoot 6)

testSquareRoot2 : IO ()
testSquareRoot2 = assertEq 50.0 (squareRoot 2500)

testSquareRoot3 : IO ()
testSquareRoot3 = assertEq 50.009999000199954 (squareRoot 2501)

-- --------------------------------------------------------------------- [ EOF ]
