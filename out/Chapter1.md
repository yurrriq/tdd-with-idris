# 1.3.4 Introductory Exercises

1.
```idris
Vect n elem -> Vect n elem
```
could be `reverse`.

2.
```idris
Vect n elem -> Vect (n * 2) elem
```
could be `join (++)`.

3.
```idris
Vect (1 + n) elem -> Vect n elem
```
could be `drop 1`.

4.
```idris
Bounded n, Vect n elem
```
could be `flip (!!)`.
