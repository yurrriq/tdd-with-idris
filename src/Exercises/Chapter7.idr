module Exercises.Chapter7

import Exercises.Chapter4 -- For Shape

%access public export


-- Exercise 7.1.6.1

Eq Shape where
  (==) (Triangle  b1 h1) (Triangle  b2 h2) = b1 == b2 && h1 == h2
  (==) (Triangle  _  _)   _                = False
  (==) (Rectangle w1 h1) (Rectangle w2 h2) = w1 == w2 && h1 == h2
  (==) (Rectangle _  _)   _                = False
  (==) (Circle    r1)    (Circle r2)       = r1 == r2
  (==) (Circle    _)      _                = False


-- Exercise 7.1.6.2

Ord Shape where
 compare x y = compare (area x) (area y)


data Expr num = Val num
              | Add (Expr num) (Expr num)
              | Sub (Expr num) (Expr num)
              | Mul (Expr num) (Expr num)
              | Div (Expr num) (Expr num)
              | Abs (Expr num)

eval : (Integral num, Neg num) => Expr num -> num
eval (Val x)   = x
eval (Add x y) = eval x + eval y
eval (Sub x y) = eval x - eval y
eval (Mul x y) = eval x * eval y
eval (Div x y) = eval x `div` eval y
eval (Abs x)   = abs (eval x)

Num ty => Num (Expr ty) where
    (+) = Add
    (*) = Mul

    fromInteger = Val . fromInteger

Neg ty => Neg (Expr ty) where
    negate x = 0 - x
    (-) = Sub
    abs = Abs

Cast (Maybe elem) (List elem) where
     cast Nothing  = []
     cast (Just x) = [x]


-- Exercise 7.2.4.1

Show num => Show (Expr num) where
  show (Val x)   = show x
  show (Add x y) = "(" ++ show x ++ " + " ++ show y ++ ")"
  show (Sub x y) = "(" ++ show x ++ " - " ++ show y ++ ")"
  show (Mul x y) = "(" ++ show x ++ " * " ++ show y ++ ")"
  show (Div x y) = "(" ++ show x ++ " / " ++ show y ++ ")"
  show (Abs x)   = "|" ++ show x ++ "|"


-- Exercise 7.2.4.2

(Eq num, Integral num, Neg num) => Eq (Expr num) where
  (==) x y = eval x == eval y

-- textExprEq1 : True  = the (Expr _) (2 + 4) == 3 + 3
-- textExprEq2 : False = the (Expr _) (2 + 4) == 3 + 4


-- Exercise 7.2.4.3

(Integral num, Neg num) => Cast (Expr num) num where
  cast = eval


-- Exercise 7.3.4.1

Functor Expr where
  map f (Val x)   = Val (f x)
  map f (Add x y) = Add (map f x) (map f y)
  map f (Sub x y) = Sub (map f x) (map f y)
  map f (Mul x y) = Mul (map f x) (map f y)
  map f (Div x y) = Div (map f x) (map f y)
  map f (Abs x)   = Abs (map f x)


-- Exercise 7.3.4.2

namespace MyVect
  data Vect : Nat -> Type -> Type where
    Nil  : Vect 0 a
    (::) : (x : a) -> (xs : Vect k a) -> Vect (S k) a

  Eq a => Eq (Vect n a) where
    (==) []      []      = True
    (==) (x::xs) (y::ys) = (x == y) && (xs == ys)

  Foldable (Vect n) where
    foldl _ z []      = z
    foldl f z (x::xs) = foldl f (f z x) xs
    foldr _ z []      = z
    foldr f z (x::xs) = f x $ foldr f z xs
