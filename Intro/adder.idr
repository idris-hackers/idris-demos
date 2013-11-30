module adder

adderTy : Nat -> Type
adderTy Z = Nat 
adderTy (S k) = Nat -> adderTy k

adder : (k : Nat) -> Nat -> adderTy k
adder Z     acc = acc
adder (S k) acc = \ x => adder k (x + acc)

