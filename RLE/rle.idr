module Main

data RLE : Vect n Char -> Type where
     REnd  : RLE []
     RChar : {xs : Vect k Char} ->
             (n : Nat) -> (c : Char) -> (rs : RLE xs) ->
             RLE (replicate (S n) c ++ xs)

------------

rle : (xs : Vect n Char) -> RLE xs
rle [] = REnd
rle (x :: xs) with (rle xs)
  rle (x :: []) | REnd = RChar 0 x REnd
  rle (x :: (c :: (replicate n c ++ xs1))) | (RChar n c rs) with (decEq x c)
    rle (x :: (x :: (replicate n x ++ xs1))) | (RChar n x rs) | (Yes refl) 
               = RChar (S n) x rs
    rle (x :: (c :: (replicate n c ++ xs1))) | (RChar n c rs) | (No var) 
               = RChar Z x (RChar n c rs)

compress : Vect n Char -> String
compress xs with (rle xs)
  compress [] | REnd = ""
  compress (c :: (replicate n c ++ xs1)) | (RChar n c rs) 
       = show (the Integer (cast (S n))) ++
           strCons c (compress xs1)

compressString : String -> String
compressString xs = compress (fromList (unpack xs))

main : IO ()
main = putStrLn (compressString "foooobaaaarbaaaz")

