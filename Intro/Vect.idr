module Vect

{-
To write this function in vim:

\d to make a template definition
\c over a variable to do case analysis on that variable
\o to fill in a hole with the 'obvious' value

-}

vzipWith : (a -> b -> c) -> Vect n a -> Vect n b -> Vect n c

