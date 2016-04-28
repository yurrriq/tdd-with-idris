hello.lidr
==========

```idris
module Main
main : IO ()
```

Add a hole to stand in place of a conversion.

```idris
main = putStrLn (?convert 'x')
```

Reload and Type Check
=====================

Reload the module in a running REPL.

``` idris
*src/hello> :r
Type checking ./src/hello.lidr
Holes: Main.convert
```

Check the type of the `convert` hole.

``` idris
*src/hello> :t convert
--------------------------------------
convert : Char -> String
Holes: Main.convert
```

As expected, the success typing of `convert` is `Char -> String`.
