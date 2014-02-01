module Main

import Effect.Select

no_attack : (Int, Int) -> (Int, Int) -> Bool
no_attack (x, y) (x', y') 
   = x /= x' && y /= y' && abs (x - x') /= abs (y - y')

rowsIn : Int -> List (Int, Int) -> List Int
rowsIn col qs = [ x | x <- [1..8], all (no_attack (x, col)) qs ]

addQueens : Int -> List (Int, Int) -> { [SELECT] } Eff m (List (Int, Int)) 
addQueens 0   qs = return qs
addQueens col qs = do row <- select (rowsIn col qs)
                      addQueens (col - 1) ((row, col) :: qs)

getQueens : Maybe (List (Int, Int))
getQueens = run (addQueens 8 [])

main : IO ()
main = do let qs = getQueens
          putStrLn ("Solution:\n" ++ show qs)

--           let num = the Integer (cast (length qs))
--           putStrLn (show num ++ " solutions:\n" ++ showAll qs)
--     where showAll [] = ""
--           showAll (x :: xs) = show x ++ "\n" ++ showAll xs 

