module Main

rep : (n : Nat) -> a -> List a
rep Z x = []
rep (S k) x = x :: rep k x

data RLE : List Char -> Type where
     REnd  : RLE []
     RChar : (n : Nat) -> (c : Char) -> (rs : RLE xs) ->
             RLE (rep n c ++ xs)

------------

rle : (xs : List Char) -> RLE xs
rle [] = REnd
rle (x :: xs) with (rle xs)
  rle (x :: []) | REnd = RChar 1 x REnd
  rle (x :: (rep n c ++ ys)) | (RChar n c rs) with (decEq x c)
    rle (x :: (rep n x ++ ys)) | (RChar n x rs) | (Yes Refl) 
             = RChar (S n) x rs
    rle (x :: (rep n c ++ ys)) | (RChar n c rs) | (No f) 
             = RChar 1 x (RChar n c rs)

compress : List Char -> String
compress xs with (rle xs)
  compress [] | REnd = "" 
  compress (rep n c ++ ys) | (RChar n c rs) 
         = show n ++ strCons c (compress ys)

compressString : String -> String
compressString xs = compress (unpack xs)

main : IO ()
main = putStrLn (compressString "foooobaaaarbaaaz")
