module Main

import Effects
import Effect.State
import Effect.Exception
import Effect.Random
import Effect.StdIO

data Expr = Var String
          | Val Integer
          | Add Expr Expr
          | Random Integer

Env : Type
Env = List (String, Integer)

getRnd : Integer -> Eff Integer [RND, STDIO]
getRnd upper = rndInt 0 upper

eval : Expr -> Eff Integer [STDIO, EXCEPTION String, STATE Env, RND]
eval (Var x) 
   = case lookup x !get of
          Nothing => raise ("No such variable " ++ x)
          Just val => return val
eval (Val x) = return x
eval (Add l r) = return (!(eval l) + !(eval r))
eval (Random x) = do val <- getRnd x
                     putStrLn (show val)
                     return val

testExpr : Expr
testExpr = Add (Add (Var "foo") (Val 42)) (Random 100)

runEval : List (String, Integer) -> Expr -> IO Integer
runEval args expr = run (eval' expr)
  where eval' : Expr -> Eff Integer [EXCEPTION String, RND, STDIO, STATE Env]
        eval' e = do put args
                     srand 1234
                     eval e

main : IO ()
main = do putStr "Number: "
          x <- getLine
          val <- runEval [("foo", cast x)] testExpr
          putStrLn $ "Answer: " ++ show val
          val <- runEval [("foo", cast x)] testExpr
          putStrLn $ "Answer: " ++ show val

