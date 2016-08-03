module Exercises.Chapter4

import Data.Vect

%default total
%access  public export

||| A binary search tree.
data BSTree : Type -> Type where
  Empty : Ord elem => BSTree elem
  Node  : Ord elem => (left  : BSTree elem) -> (val : elem) ->
                      (right : BSTree elem) -> BSTree elem

{-
data OrdList : Type -> Type where
  Nil  : Ord elem => OrdList elem
  (::) : Ord elem => elem -> OrdList elem -> OrdList elem
implementation Foldable OrdList where
  foldr f e []      = e
  foldr f e (x::xs) = f x (foldr f e xs)
  foldl f e []      = e
  foldl f e (x::xs) = foldl f (f e x) xs

fromList : Ord a => (xs : List a) -> OrdList a
fromList []        = []
fromList (x :: xs) = x :: fromList xs
-}

insert : elem -> BSTree elem -> BSTree elem
insert x Empty                      = Node Empty x Empty
insert x orig@(Node left val right) =
  case compare x val of
    LT => Node (insert x left) val right
    EQ => orig
    GT => Node left val (insert x right)


-- Exercise 4.1.5.1

listToTree : Ord a => List a -> BSTree a
listToTree = foldl (flip insert) Empty


-- Exercise 4.1.5.2

treeToList : BSTree a -> List a
treeToList Empty                 = []
treeToList (Node left val right) = treeToList left ++ [val] ++ treeToList right


-- Exercise 4.1.5.3

||| An integer arithemtic expression.
data Expr : Type where
  ||| A single integer.
  Exp : (n : Int)          -> Expr
  ||| Addition of an expression to an expression.
  (+) : (lhs : Expr) -> (rhs : Expr) -> Expr
  ||| Subtraction of an expression from an expresison.
  (-) : (lhs : Expr) -> (rhs : Expr) -> Expr
  ||| Multiplication of an expression with an expression.
  (*) : (lhs : Expr) -> (rhs : Expr) -> Expr


-- Exercise 4.1.5.4

||| Evaluate an integer arithemtic expression.
evaluate : Expr -> Int
evaluate (Exp n)     = n
evaluate (lhs + rhs) = evaluate lhs + evaluate rhs
evaluate (lhs - rhs) = evaluate lhs - evaluate rhs
evaluate (lhs * rhs) = evaluate lhs * evaluate rhs


-- Exercise 4.1.5.5

||| Return the larger of two inputs, or `Nothing` if both inputs are `Nothing`.
maxMaybe : Ord a => Maybe a -> Maybe a -> Maybe a
maxMaybe mx my = (max <$> mx <*> my) <|> mx <|> my


-- Exercise 4.1.5.6

||| A shape is a triangle, rectangle or circle.
data Shape : Type where
  Triangle  : (base,  height : Double) -> Shape
  Rectangle : (width, height : Double) -> Shape
  Circle    : (radius        : Double) -> Shape

%name Shape shape, shape1, shape2

||| Calculate the area of a shape.
area : Shape -> Double
area (Triangle  base  height) = 0.5 * base * height
area (Rectangle width height) = width * height
area (Circle    radius)       = pi * pow radius 2


||| A picture.
data Picture = ||| A primitive shape.
               Primitive Shape
             | ||| A combination of two other pictures.
               Combine   Picture Picture
             | ||| A picture rotated through an angle.
               Rotate    Double  Picture
             | ||| A picture translated to a different location.
               Translate Double  Double Picture

%name Picture pic, pic1, pic2

picture_area : Picture -> Double
picture_area (Primitive shape)   = area shape
picture_area (Combine pic pic1)  = picture_area pic + picture_area pic1
picture_area (Rotate x pic)      = picture_area pic
picture_area (Translate x y pic) = picture_area pic

||| Return the area of the biggest triangle in a picture,
||| or `Nothing` if there are no triangles.
biggestTriangle : Picture -> Maybe Double
biggestTriangle (Primitive shape)   = case shape of
                                        Triangle _ _ => Just    $ area shape
                                        _            => Nothing
biggestTriangle (Combine pic pic1)  = maxMaybe (biggestTriangle pic)
                                               (biggestTriangle pic1)
biggestTriangle (Rotate _ pic)      = biggestTriangle pic
biggestTriangle (Translate _ _ pic) = biggestTriangle pic


--- Exercise 4.2.4.1-2

data PowerSource = Petrol | Electricity | Pedal

data Vehicle : PowerSource -> Type where
  Unicycle         : Vehicle Pedal
  Bicycle          : Vehicle Pedal
  GalacticPizzaCar : Vehicle Electricity
  Motorcycle       : (fuel : Nat) -> Vehicle Petrol
  Car              : (fuel : Nat) -> Vehicle Petrol
  Bus              : (fuel : Nat) -> Vehicle Petrol

wheels : Vehicle power -> Nat
wheels Unicycle         = 1
wheels Bicycle          = 2
wheels GalacticPizzaCar = 3
wheels (Motorcycle _)   = 2
wheels (Car        _)   = 4
wheels (Bus        _)   = 4

refuel : Vehicle Petrol -> Vehicle Petrol
refuel (Motorcycle fuel) = Motorcycle  50
refuel (Car        fuel) = Car        100
refuel (Bus        fuel) = Bus        200
refuel Unicycle          impossible


-- Exercise 4.2.4.3-4

namespace MyVect
  take : (n : Nat) -> Vect (n + m) a -> Vect n a
  take  Z     _        = []
  take (S k) (x :: xs) = x :: MyVect.take k xs


-- Exercise 4.2.4.5

-- tryIndex : Integer -> Vect n a -> Maybe a
-- tryIndex {n} i xs = case integerToFin i n of
--                       Nothing  => Nothing
--                       Just j   => Just (index j xs)

on : (b -> b -> c) -> (a -> b) -> a -> a -> c
on g f = \x,y => f x `g` f y

sumEntries : Num a => (pos : Integer) -> Vect n a -> Vect n a -> Maybe a
sumEntries {n} pos xs ys with (integerToFin pos n)
  | m = (\i => ((+) `on` index i) xs ys) <$> m
-- sumEntries pos xs ys = (+) <$> tryIndex pos xs <*> tryIndex pos ys
-- sumEntries {n} pos xs ys = case integerToFin pos n of
--                           Nothing  => Nothing
--                           Just pos => Just $ index pos xs + index pos ys
