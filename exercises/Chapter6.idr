module Chapter6

import Data.Vect

%access public export


-- Exercise 6.2.3.1

||| An `n` by `m` matrix.
Matrix : (n, m : Nat) -> Type
Matrix n m = Vect n (Vect m Double)


-- Exercise 6.2.3.2

data Format = Number     Format
            | Dbl        Format
            | Chr        Format
            | Str        Format
            | Lit String Format
            | End

PrintfType : Format -> Type
PrintfType (Number  fmt) = (i : Int)    -> PrintfType fmt
PrintfType (Dbl     fmt) = (i : Double) -> PrintfType fmt
PrintfType (Chr     fmt) = (c : Char)   -> PrintfType fmt
PrintfType (Str     fmt) = (s : String) -> PrintfType fmt
PrintfType (Lit str fmt) = PrintfType fmt
PrintfType  End          = String

printfFmt : (fmt : Format) -> (acc : String) -> PrintfType fmt
printfFmt (Number  fmt) acc = printfFmt fmt . (acc ++) . show
printfFmt (Dbl     fmt) acc = printfFmt fmt . (acc ++) . show
printfFmt (Chr     fmt) acc = printfFmt fmt . (acc ++) . flip strCons ""
printfFmt (Str     fmt) acc = printfFmt fmt . (acc ++)
printfFmt (Lit lit fmt) acc = printfFmt fmt (acc ++ lit)
printfFmt  End          acc = acc

toFormat : (xs : List Char) -> Format
toFormat []                    = End
toFormat ('%' :: 'd' :: chars) = Number  (toFormat chars)
toFormat ('%' :: 'f' :: chars) = Dbl     (toFormat chars)
toFormat ('%' :: 'c' :: chars) = Chr     (toFormat chars)
toFormat ('%' :: 's' :: chars) = Str     (toFormat chars)
toFormat ('%'        :: chars) = Lit "%" (toFormat chars)
toFormat ( c         :: chars) with (toFormat chars)
  | (Lit lit chars') = Lit (strCons c lit) chars'
  |  fmt             = Lit (strCons c "")  fmt

printf : (fmt : String) -> PrintfType (toFormat (unpack fmt))
printf fmt = printfFmt _ ""


-- Exercise 6.2.3.3

TupleVect : (k : Nat) -> (a : Type) -> Type
TupleVect  Z    a = ()
TupleVect (S k) a = (a, TupleVect k a)

test : TupleVect 4 Nat
test = (1,2,3,4,())
