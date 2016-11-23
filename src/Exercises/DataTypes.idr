-- ----------------------------------------------------------- [ DataTypes.idr ]
-- Module      : Exercises.DataTypes
-- Description : Solutions to the Chapter 4 exercises in Edwin Brady's
--               book, "Type-Driven Development with Idris."
-- --------------------------------------------------------------------- [ EOH ]
module Exercises.DataTypes

import public Chapter.DataTypes
import public Data.Vect

%access export

on : (b -> b -> c) -> (a -> b) -> a -> a -> c
on g f = \x,y => f x `g` f y

onIdentity : -- ((*) : b -> b -> c) ->
             ((*) `on` Basics.id) x y = (*) x y
onIdentity = Refl

onComposition : -- ((*) : c -> c -> d) -> (f : b -> c) -> (g : a -> b) -> 
                (((*) `on` f) `on` g) x y = ((*) `on` (f . g)) x y
onComposition = Refl 

flipOn : (a -> b) -> (b -> b -> c) -> a -> a -> c
flipOn = flip on

onFlip : -- (f : a -> b) -> (g : b -> c) ->
         (flipOn f . flipOn g) h x y = (flipOn (g . f)) h x y
onFlip = Refl

-- --------------------------------------------------------- [ 4.1.5 Exercises ]

||| Insert every element of a list into a binary search tree.
|||
||| ```idris example
||| listToTree [1,4,3,5,2]
||| ```
listToTree : Ord a => List a -> Tree a
listToTree = foldr insert Empty

||| Flatten a tree into a list using *inorder* traversal.
|||
||| ```idris example
||| treeToList (listToTree [4,1,8,7,2,3,9,5,6])
||| ```
treeToList : Tree a -> List a
treeToList Empty = []
treeToList (Node l x r) = treeToList l ++ [x] ++ treeToList r

||| An integer arithmetic expression.
public export
data Expr = ||| A single integer.
            Val Int
          | ||| Addition of an expression to an expression.
            Add Expr Expr
          | ||| Subtraction of an expression from an expresison.
            Sub Expr Expr
          | ||| Multiplication of an expression with an expression.
            Mult Expr Expr

||| Evaluate an integer arithmetic expression.
|||
||| ```idris example
||| evaluate (Mult (Val 10) (Add (Val 6) (Val 3)))
||| ```
evaluate : Expr -> Int
evaluate (Val x)    = x
evaluate (Add x y)  = evaluate x + evaluate y
evaluate (Sub x y)  = evaluate x - evaluate y
evaluate (Mult x y) = evaluate x * evaluate y

||| Return the larger of two inputs, or `Nothing` if both inputs are `Nothing`.
|||
||| ```idris example
||| maxMaybe (Just 4) (Just 5)
||| ```
||| ```idris example
||| maxMaybe (Just 4) Nothing
||| ```
maxMaybe : Ord a => Maybe a -> Maybe a -> Maybe a
maxMaybe mx my = (max <$> mx <*> my) <|> mx <|> my

-- --------------------------------------------------------- [ 4.2.4 Exercises ]

public export
data PowerSource = Petrol
                 | Electricity
                 | Pedal

public export
data Vehicle : PowerSource -> Type where
     Unicycle : Vehicle Pedal
     Bicycle : Vehicle Pedal
     GalacticPizzaCar : Vehicle Electricity
     Motorcycle : (fuel : Nat) -> Vehicle Petrol
     Car : (fuel : Nat) -> Vehicle Petrol
     Bus : (fuel : Nat) -> Vehicle Petrol

||| Return the number of wheels on a vehicle.
|||
||| ```idris example
||| wheels GalacticPizzaCar
||| ```
wheels : Vehicle power -> Nat
wheels Unicycle         = 1
wheels Bicycle          = 2
wheels GalacticPizzaCar = 3
wheels (Motorcycle _)   = 2
wheels (Car _)          = 4
wheels (Bus _)          = 4

||| Refuel a petrol-powered vehicle.
|||
||| ```idris example
||| refuel (Car 0)
||| ```
refuel : Vehicle Petrol -> Vehicle Petrol
refuel (Motorcycle _) = Motorcycle  50
refuel (Car _)        = Car 100
refuel (Bus _)        = Bus 200
-- NOTE: refuel Unicycle impossible

||| Get the first n elements of a Vect.
|||
||| ```idris example
||| vectTake 3 [1,2,3,4,5,6,7]
||| ```
|||
||| @ n the number of elements to take
vectTake : (n : Nat) -> Vect (n + m) a -> Vect n a
vectTake Z _             = []
vectTake (S k) (x :: xs) = x :: vectTake k xs

||| Return `Just` the sum of the entries at position `pos` in each of the
||| inputs, if `pos` is within bounds, or `Nothing` otherwise.
|||
||| ```idris example
||| sumEntries 2 [1,2,3,4] [5,6,7,8]
||| ```
||| ```idris example
||| sumEntries 4 [1,2,3,4] [5,6,7,8]
||| ```
|||
||| @ pos a possibly out-of-bounds index into the input vectors
sumEntries : Num a => (pos : Integer) -> Vect n a -> Vect n a -> Maybe a
sumEntries {n} pos xs ys with (integerToFin pos n)
  | m = pure $ ((+) `on` index !m) xs ys

-- --------------------------------------------------------------------- [ EOF ]
