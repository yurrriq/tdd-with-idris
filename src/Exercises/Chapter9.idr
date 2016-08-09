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

  notTha1 : Not (x = value) -> Not (Last [x] value)
  notTha1 contra  LastOne       = contra Refl
  notTha1 _      (LastCons nil) = notLastNil nil

  noWay : Not (xs = []) -> Not (Last xs value) -> Not (Last (x :: xs) value)
  noWay contra _       LastOne       = absurd (contra Refl)
  noWay _      contra (LastCons prf) = contra prf

  isLast : DecEq a => (xs : List a) -> (value : a) -> Dec (Last xs value)
  isLast [] _ = No notLastNil
  isLast (x :: xs) value with (isLast xs value)
    | Yes prf = Yes (LastCons prf)
    | No callate with (decEq xs [])
      isLast (x::_) value | _ | Yes Refl with (decEq x value)
        isLast [_] _ | _ | _ | Yes Refl = Yes LastOne
        | No sucka = No (notTha1 sucka)
      | No jose = No (noWay jose callate)
