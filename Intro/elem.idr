module elem

data Elem : a -> List a -> Type where
     Here : Elem x (x :: xs)
     There : Elem x xs -> Elem x (y :: xs)

elem_test : Elem 2 [1..4]
elem_test = There Here

isElem : DecEq a => (x : a) -> (xs : List a) -> Maybe (Elem x xs)

