module Main

import Effect.State

data Tree a = Leaf 
            | Node (Tree a) a (Tree a)

flattenTree : Tree a -> List a
flattenTree Leaf = []
flattenTree (Node l x r) = flattenTree l ++ (x :: flattenTree r)

testTree : Tree String
testTree = Node (Node Leaf "One" (Node Leaf "Two" Leaf))
                "Three"
                (Node (Node Leaf "Four" Leaf) "Five" Leaf)

data Tag : Type where
data Leaves : Type where

label : Tree a -> { [Leaves ::: STATE Int, Tag ::: STATE Int] } 
                  Eff m (Tree (Int, a))
label Leaf = do Leaves :- update (+1)
                return Leaf
label (Node l x r) = do l' <- label l 
                        lbl <- Tag :- get
                        Tag :- put (lbl + 1)
                        r' <- label r
                        return (Node l' (lbl, x) r')

main : IO ()
main = do -- let ([Leaves := l, _], x) 
          let x = runPureInit [default, Tag := 1] (label testTree)
          print (flattenTree x)














{-
label Leaf = return Leaf
label (Node l x r) = do l' <- label l 
                        lbl <- get
                        put (lbl + 1)
                        r' <- label r
                        return (Node l' (lbl, x) r')
-}
