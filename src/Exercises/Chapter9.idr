module Exercises.Chapter9

%access  public export
%default total

namespace List

  -- Exercise 9.1.6.1

  data Elem  : a -> List a -> Type where
       Here  : Elem x (x :: xs)
       There : (later : Elem x xs) -> Elem x (y :: xs)


  -- Exercise 9.1.6.2

  data Last     : List a -> a -> Type where
       LastOne  : Last [value] value
       LastCons : (prf : Last xs value) -> Last (x :: xs) value

  notLastNil : Not (Last [] value)
  notLastNil LastOne      impossible
  notLastNil (LastCons _) impossible

  notTha1 :(contra : Not (value = x)) -> Not (Last [x] value)
  notTha1 contra LastOne        = contra Refl
  notTha1 contra (LastCons nil) = notLastNil nil

  noWay :  (jose : Not (xs = []))
        -> (contra : Not (Last xs value))
        -> Not (Last (x :: xs) value)
  noWay jose contra LastOne        = void (jose Refl)
  noWay jose contra (LastCons prf) = contra prf

  isLast : DecEq a => (xs : List a) -> (value : a) -> Dec (Last xs value)
  isLast [] value = No notLastNil
  isLast (x :: xs) value with (isLast xs value)
    | Yes prf = Yes (LastCons prf)
    | No contra with (decEq xs [])
      | Yes prf with (decEq value x)
        | Yes yep = rewrite prf in rewrite yep in Yes LastOne
        | No nope = rewrite prf in No (notTha1 nope)
      | No jose = No (noWay jose contra)
