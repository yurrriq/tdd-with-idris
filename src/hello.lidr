= hello.lidr

> module Main
> main : IO ()

The following results in a type error, since a `Char` is not a `String`.

> main = putStrLn 'x'

= Reload and Explode

Reload the module in a running REPL.

```idris
*src/hello> :r
Type checking ./src/hello.lidr
./src/hello.lidr:8:17:When checking right hand side of main with expected type
        IO ()

When checking an application of function Prelude.Interactive.putStrLn:
        Type mismatch between
                Char (Type of 'x')
        and
                String (Expected type)
Holes: Main.main
```
