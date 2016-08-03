module Test.Chapter7

import Test
import Exercises.Chapter7

%access public export

testFunctorExpr1 : IO ()
testFunctorExpr1 =
 assertEq (map (*2) (the (Expr _) (1 + 2 * 3)))
          (Add (Val 2) (Mul (Val 4) (Val 6)))

-- testFunctorExpr2 :
--   map show (the (Expr _) (1 + 2 * 3)) = Add (Val "1") (Mul (Val "2") (Val "3"))
