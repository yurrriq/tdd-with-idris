= hello.lidr

The module header is rather straightforward.

> module Main

Function declarations are a lot like Haskell, except `::` is replaced with `:`.

> main : IO ()

Function definitions, too, are straightforward.

> main = putStrLn "Hello, Idris World!"


= Compile

To compile this file:

```fish
$ idris src/hello.idr -o bin/hello
```

... or use `make`:

```fish
$ make bin/hello
```

= Run

```fish
$ bin/hello
```

```
Hello, Idris World!
```
