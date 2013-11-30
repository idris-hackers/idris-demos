module elem

using (xs : List a)
  data Elem : a -> List a -> Type where
       Here : Elem x (x :: xs)
       There : Elem x xs -> Elem x (y :: xs)

isElem : DecEq a => (x : a) -> (xs : List a) -> Maybe (Elem x xs)
isElem x [] = Nothing
isElem x (t :: ts) with (decEq x t)
  isElem x (x :: ts) | (Yes refl) = Just Here
  isElem x (t :: ts) | (No var) = Just (There !(isElem x ts))

