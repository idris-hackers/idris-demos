module Starfield

-- Background starfield effect

import Effects
import Effect.SDL
import Effect.StdIO
import Effect.State

import Rnd

data Starfield : Type where -- for labelling state

StarEff : Type -> Type
StarEff t = { [Starfield ::: STATE (List (Int, Int)), RND] } Eff t

initStarfield : List (Int, Int) -> Nat -> StarEff ()
initStarfield acc Z = Starfield :- put acc
initStarfield acc n 
    = do x <- rndInt 0 639
         y <- rndInt 0 479
         initStarfield ((x, y) :: acc) (n - 1)

updateStarfield : StarEff ()
updateStarfield = do xs <- Starfield :- get
                     xs' <- upd [] xs
                     Starfield :- put xs'
 where
  upd : List (Int, Int) -> List (Int, Int) -> { [RND] } Eff (List (Int, Int))
  upd acc [] = return acc
  upd acc ((x, y) :: xs)
      = if (y > 479) then do
             x <- rndInt 0 639
             upd ((x, 0) :: acc) xs
           else
             upd ((x, y+1) :: acc) xs

drawStarfield : List (Int, Int) -> { [SDL_ON] } Eff ()
drawStarfield [] = return ()
drawStarfield ((x, y) :: xs) = do line white x y x y
                                  drawStarfield xs

