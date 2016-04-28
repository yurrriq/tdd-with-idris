hello.lidr
==========

The module header is rather straightforward.

```idris
module Main
```

Function declarations are a lot like Haskell, except `::` is replaced with `:`.

```idris
main : IO ()
```

Function definitions, too, are straightforward.

```idris
main = putStrLn "Hello, Idris World!"
```

Compile
=======

To compile this file:

``` fish
$ idris src/hello.idr -o bin/hello
```

... or use `make`:

``` fish
$ make bin/hello
```

Run
===

``` fish
$ bin/hello
```

    Hello, Idris World!
