= Chapter 3

> module Exercises.Chapter3


Import `Data.Vect`, because we'll need it later.

> import Data.Vect


Have Idris check that each function is total.

> %default total

Export functions by default.

> %access public export


== 3.2.4 Exercises

=== 1. `length : List a -> Nat`

> my_length : List a -> Nat

Move the point over `my_length` and press `M-RET d` to add a clause.

```idris
my_length xs = ?my_length_rhs
```

Move the point over `xs` and press `C-c C-c` to case split on it.

```idris
my_length [] = ?my_length_rhs_1
my_length (x :: xs) = ?my_length_rhs_2
```

With the point on `?my_length_rhs_1`, press `M-RET p` to have Idris fill in `0`.

> my_length [] = 0

Fill out `?my_length_rhs_2` manually.

> my_length (x :: xs) = S (my_length xs)


=== 2. `reverse : List a -> List a`

> my_reverse : List a -> List a

`M-RET d` on `my_reverse` to add a clause.

```idris
my_reverse xs = ?my_reverse_rhs
```

Case split on `xs` (`C-c C-c`).

```idris
my_reverse [] = ?my_reverse_rhs_1
my_reverse (x :: xs) = ?my_reverse_rhs_2
```

Have Idris fill in `?my_reverse_rhs_1`.

```idris
my_reverse [] = []
```

We're actually going to need to do some fold-like recursion here, so revert back
to `my_reverse xs = ?my_reverse_rhs` and add a `where` clause with a local
function, `go`.

```idris
my_reverse xs = ?my_reverse_rhs
  where go : List a -> List a -> List a
```

Add a clause for `go` (`M-RET d`).

```idris
my_reverse xs = ?my_reverse_rhs
  where
    go : List a -> List a -> List a
    go xs ys = ?go_rhs
```

Case split on `xs` (`C-c C-c`).

```idris
go [] ys = ?go_rhs_1
go (x :: xs) ys = ?go_rhs_2
```

Rename `ys` to `acc`(umulator).

```idris
go [] acc = ?go_rhs_1
go (x :: xs) acc = ?go_rhs_2
```

Have Idris fill in `?go_rhs_1` (`M-RET p`).

```idris
go [] acc = acc
```

Fill in `?go_rhs_2` and `?my_reverse_rhs` manually. Rename the top-level `xs`
to `lst` to avoid confusion/shadowing.

> my_reverse lst = go lst []
>   where
>     go : List a -> List a -> List a
>     go [] acc = acc
>     go (x :: xs) acc = go xs (x :: acc)


=== 3. `map : (a -> b) -> List a -> List b`

> namespace List
>   map : (a -> b) -> List a -> List b

Press `M-RET d` on `map` to add a clause.

```idris
  map f xs = ?map_rhs
```

Case split (`C-c C-c`) on `xs`.

```idris
  map f [] = ?map_rhs_1
  map f (x :: xs) = ?map_rhs_2
```

Have Idris fill in `?map_rhs_1` (`M-RET p`).

>   map f [] = []

Fill in `?map_rhs_2` manually.

>   map f (x :: xs) = f x :: map f xs


=== 4. `map : (a -> b) -> Vect n a -> Vect n b`

> namespace Vect
>   map : (a -> b) -> Vect n a -> Vect n b

Add a `map` clause (`M-RET d`).

```idris
  map f xs = ?map_rhs
```

~~Case split (`C-c C-c`) on `xs`.~~

N.B. That doesn't seem to work, for whatever reason,
so add the clauses manually.

```idris
  map f [] = ?map_rhs_1
  map f (x :: xs) = ?map_rhs_2
```

Emacs or Idris seems upset about the holes too, so fill them in manually.

>   map f [] = []
>   map f (x :: xs) = f x :: map f xs


== 3.3.3 Exercises


=== 1. Reimplement `transpose_mat` with `zipWith` instead of `transpose_helper`

*Type* `transpose_mat`.

```idris
transpose_mat : Vect m (Vect n elem) -> Vect n (Vect m elem)
```

*Define* `transpose_mat`.

N.B. Emacs or Idris is still upset, so this is not very interactive.

```idris
transpose_mat [] = ?transpose_mat_rhs_1
transpose_mat (x :: xs) = ?transpose_mat_rhs_2
```

*Type* `?transpose_mat_rhs_1`.

```idris
  elem : Type
     n : Nat
--------------------------------------
transpose_mat_rhs_1 : Vect n (Vect 0 elem)
```

Rename `?transpose_mat_rhs_1` to `?create_empties` and make a lemma (a.k.a. lift
it to a top-level function) with `M-RET i e`.

```idris
create_empties : Vect n (Vect 0 elem)

transpose_mat : Vect m (Vect n elem) -> Vect n (Vect m elem)
transpose_mat [] = create_empties
transpose_mat (x :: xs) = ?transpose_mat_rhs_2
```

*Type* `?transpose_mat_rhs_2`.

```idris
  elem : Type
     n : Nat
     x : Vect n elem
     k : Nat
    xs : Vect k (Vect n elem)
--------------------------------------
transpose_mat_rhs_2 : Vect n (Vect (S k) elem)
```

(Further) *define* `transpose_mat`.

```idris
create_empties : Vect n (Vect 0 elem)

transpose_mat : Vect m (Vect n elem) -> Vect n (Vect m elem)
transpose_mat [] = create_empties
transpose_mat (x :: xs) = let xs_trans = transpose_mat xs
                          in ?transpose_mat_rhs_2
```

Fill in `?transpose_mat_rhs_2` with `zipWith
and do away with the intermediate variable, `xs_trans`.

```idris
create_empties : Vect n (Vect 0 elem)

transpose_mat : Vect m (Vect n elem) -> Vect n (Vect m elem)
transpose_mat [] = create_empties
transpose_mat (x :: xs) = zipWith (::) x (transpose_mat xs)
```

*Define* `create_empties`.

> create_empties : Vect n (Vect 0 elem)
> create_empties {n} = replicate n []
>
> transpose_mat : Vect m (Vect n elem) -> Vect n (Vect m elem)
> transpose_mat [] = create_empties
> transpose_mat (x :: xs) = zipWith (::) x (transpose_mat xs)


=== 2. `addMatrix`

> addMatrix : Num numType =>
>             Vect rows (Vect cols numType) -> Vect rows (Vect cols numType) ->
>             Vect rows (Vect cols numType)
> addMatrix []          ys          = ys
> addMatrix (xs :: xxs) (ys :: yys) = zipWith (+) xs ys :: addMatrix xxs yys

=== 3. `multMatrix`

> multMatrix : Num numType =>
>              Vect n (Vect m numType) -> Vect m (Vect p numType) ->
>              Vect n (Vect p numType)
> multMatrix left right = map (go (transpose_mat right)) left
>   where
>     go : Vect p (Vect m numType) -> Vect m numType -> Vect p numType
>     go xs ys = map (sum . zipWith (*) ys) xs


\[
  \begin{pmatrix}
    1 &   2 \\
    3 &   4 \\
    5 &   6
  \end{pmatrix} \times
  \begin{pmatrix}
    7 &   8 &   9 &  10 \\
   11 &  12 &  13 &  14
  \end{pmatrix} =
  \begin{pmatrix}
   29 &  32 &  35 &  38 \\
   65 &  72 &  79 &  86 \\
  101 & 112 & 123 & 134
  \end{pmatrix}
\]

> testMultMatrix : [ [  1,   2]
>                  , [  3,   4]
>                  , [  5,   6] ]
>                  `multMatrix`
>                  [ [  7,   8,   9,  10]
>                  , [ 11,  12,  13,  14] ]
>                  =
>                  [ [ 29,  32,  35,  38]
>                  , [ 65,  72,  79,  86]
>                  , [101, 112, 123, 134] ]
> testMultMatrix = Refl
