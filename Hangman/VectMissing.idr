module VectMissing

using (x : a, xs : Vect n a)
  data IsElem : a -> Vect n a -> Type where
       First : IsElem x (x :: xs)
       Later : IsElem x xs -> IsElem x (y :: xs)

  instance Uninhabited (IsElem x []) where
    uninhabited First impossible
    uninhabited (Later p) impossible

  isElem : DecEq a => (x : a) -> (xs : Vect n a) -> Maybe (IsElem x xs)
  isElem x [] = Nothing
  isElem x (y :: xs) with (decEq x y)
    isElem x (x :: xs) | (Yes Refl) = Just First
    isElem x (y :: xs) | (No f) = Just (Later !(isElem x xs))

  shrink : (xs : Vect (S n) a) -> IsElem x xs -> Vect n a
  shrink (x :: ys) First = ys
  shrink (y :: []) (Later p) = absurd p
  shrink (y :: (x :: xs)) (Later p) = y :: shrink (x :: xs) p


