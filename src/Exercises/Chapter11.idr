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
totalREPL prompt action = do putStr prompt
                             input <- getLine
                             putStr (action input)
                             totalREPL prompt action

-- ------------------------------------------------------- [ Exercise 11.3.4.1 ]

private
arithInputs : (seed : Int) -> Stream Int
arithInputs seed = map bound (randoms seed)
  where
    bound : Int -> Int
    bound num with (divides num 12)
      bound ((12 * _div) + rem) | (DivBy _prf) = rem + 1

partial
quiz : IO ()
quiz = go (arithInputs 12345) 0 0
  where
    partial
    go : Stream Int -> (score, attempted : Nat) -> IO ()
    go (num1 :: num2 :: nums) score attempted = do
      putStrLn ("Score so far: " ++ show score ++ " / " ++ show attempted)
      putStr (show num1 ++ " * " ++ show num2 ++ "? ")
      guess <- getLine
      if "quit" == guess
        then putStrLn ("Final score: " ++ show score ++ " / " ++ show attempted)
        else do let answer = num1 * num2
                if cast guess == answer
                  then do putStrLn "Correct!"
                          go nums (score + 1) (attempted + 1)
                  else do putStrLn ("Wrong, the answer is " ++ show answer)
                          go nums score (attempted + 1)

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
