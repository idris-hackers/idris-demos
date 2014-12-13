module Main

{-
To write these functions in vim:

\d to make a template definition
\c over a variable to do case analysis on that variable
\o to fill in a hole with the 'obvious' value

-}

append : Vect n a -> Vect m a -> Vect (n + m) a
append [] ys = ys
append (x :: xs) ys = x :: append xs xs

vZipWith : (a -> b -> c) -> Vect n a -> Vect n b -> Vect n c

