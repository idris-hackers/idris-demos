module elem

using (xs : List a)
  data Elem : a -> List a -> Type where
       Here : Elem x (x :: xs)
       There : Elem x xs -> Elem x (y :: xs)

isElem : DecEq a => (x : a) -> (xs : List a) -> Maybe (Elem x xs)
isElem x [] = Nothing
isElem x (y :: xs) with (decEq x y)
  isElem x (x :: xs) | (Yes refl) = Just Here
  isElem x (y :: xs) | (No f) = Just (There !(isElem x xs))

