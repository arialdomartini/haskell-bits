# Functional Programming in Haskell

Notes and exercising attending the [Introduction to Functional Programming](https://www.edx.org/course/introduction-functional-programming-delftx-fp101x-0) course.

## Tricks and observations

* @lucax88x found that in ghci `:reload` and commands in general can be abbreviated, e.g. with `:r"`
* Instead of using the `ghci` REPL, one can compile the code with `ghc [filename]`; it is mandatory to define a `main` function such as

```haskell
double x = x + x

main = print (double 20)
```

otherwise the file won't compile

* Erik taught us that the use of \`, for example in

```haskel
average ns = sum ns `div` length ns
```

makes `div` an infix operator; in other words, it is syntactic sugar for switching the position of the function name and its first parameter. This is similar to nim, which allows to write

```nim
some_function(a, b)
```

or 

```nim
a.some_function(b)
```

indifferently.
