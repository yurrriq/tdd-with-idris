module Chapter8

-- import Data.Vect
import Exercises.Chapter7

%access  public export
%default total


-- Exercise 8.1.7.1

same_cons :  {xs : List a} -> {ys : List a}
          -> xs = ys -> x :: xs = x :: ys
same_cons = cong


-- Exercise 8.1.7.2

same_lists :  {xs : List a} -> {ys : List a}
           -> x = y -> xs = ys -> x :: xs = y :: ys
same_lists prf prf' = rewrite prf in rewrite prf' in Refl


-- Exercise 8.1.7.3

data ThreeEq : a -> b -> c -> Type where
  Same : (x : a) -> ThreeEq x x x


-- Exercise 8.1.7.4

allSameS : (x, y, z : Nat) -> ThreeEq x y z -> ThreeEq (S x) (S y) (S z)
allSameS z z z (Same z) = Same (S z)

-- `allSameS` can be thought of as a proof that if `x`, `y` and `z` are equal,
-- then `S x`, `S y` and `S z` are also equal.


-- Exercise 8.2.6.1

my_plusCommutes : (n, m : Nat) -> n + m = m + n
my_plusCommutes  Z    m = sym (plusZeroRightNeutral m)
my_plusCommutes (S k) m = rewrite my_plusCommutes k m in plusSuccRightSucc m k

-- my_plusCommutes n  Z    = plusZeroRightNeutral n
-- my_plusCommutes n (S k) = rewrite sym (my_plusCommutes n k) in
--                           sym (plusSuccRightSucc n k)


-- Exercise 8.2.6.2

my_reverse : Vect n a -> Vect n a
my_reverse xs = reverse' [] xs
  where
    reverse' : Vect n a -> Vect m a -> Vect (n+m) a
    reverse' {n}           acc []      = rewrite plusZeroRightNeutral n in acc
    reverse' {n} {m = S k} acc (y::ys) = rewrite sym (plusSuccRightSucc n k) in
                                         reverse' (y::acc) ys


-- Exercise 8.3.4.1

head : Vect (S n) a -> a
head (x :: _) = x

tail : Vect (S n) a -> Vect n a
tail (_ :: xs) = xs

head_unequal :  DecEq a => {xs : Vect n a} -> {ys : Vect n a}
             -> (contra : Not (x = y))
             -> Not (x :: xs = y :: ys)
head_unequal contra = void . contra . cong {f = head}

tail_unequal :  DecEq a => {xs : Vect n a} -> {ys : Vect n a}
             -> (contra : Not (xs = ys))
             -> Not (x :: xs = y :: ys)
tail_unequal contra = void . contra . cong {f = tail}


-- Exercise 8.3.4.2

namespace MyVect
  implementation DecEq a => DecEq (Vect n a) where
    decEq [] [] = Yes Refl
    decEq (x :: xs) (y :: ys) with (decEq x y)
      | Yes same_head with (decEq xs ys)
        | Yes same_tail = Yes (rewrite same_head in rewrite same_tail in Refl)
        | No  contra    = No  (tail_unequal contra)
      | No contra = No (head_unequal contra)
