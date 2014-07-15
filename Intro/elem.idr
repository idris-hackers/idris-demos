module elem

data Elem : a -> List a -> Type where
     Here : Elem x (x :: xs)
     There : Elem x xs -> Elem x (y :: xs)

isElem : DecEq a => (x : a) -> (xs : List a) -> Maybe (Elem x xs)

