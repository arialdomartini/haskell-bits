-- enter the repl and run
-- :load maths.hs
-- Then perform some operations

double x = x + x

quadruple x = double (double x)

factorial n = product [1 .. n]

average ns = sum ns `div` length ns
average1 ns = div (sum ns) (length ns)
