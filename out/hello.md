hello.lidr
==========

```idris
module Main
main : IO ()
```

Idris supports incomplete definitions with holes.

```idris
main = putStrLn ?greeting
```

In the example above, `?greeting` is a hole that can be filled by a `String`.

Interact
========

Load the module into a REPL.

``` fish
$ idris src/hello.idr
```

``` idris
Type checking ./src/hello.lidr
Holes: Main.greeting
```

Type check `greeting`

``` idris
*src/hello> :t greeting
--------------------------------------
greeting : String
Holes: Main.greeting
```

A naïve attempt to evaluate `greeting` results in an error.

``` idris
*src/hello> greeting
(input):1:1-9:INTERNAL ERROR: Can't find name greeting
This is probably a bug, or a missing error message.
Please consider reporting at https://github.com/idris-lang/Idris-dev/issues
Holes: Main.greeting
```

Note to self: Consider reporting this potential bug.

To actually evaluate `greeting`, I had to use its fully-qualified name.

``` idris
*src/hello> Main.greeting
?greeting : String
Holes: Main.greeting
```

Bye bye

``` idris
*src/hello> :q
Bye bye
```
