module Gamestate

import Effect.State
import Effect.SDL
import Effect.StdIO

import Aliens
import Rnd

record Gamestate : Type where
    MkGamestate : (position : (Int, Int)) ->
                  (xmovement : Int) ->
                  (ymovement : Int) ->
                  (bullets : List (Int, Int)) ->
                  (bombs : List (Int, Int)) ->
                  (aliens : List Alien) ->
                  Gamestate

initState : Gamestate
initState = MkGamestate (320,400) 0 0 [] [] startAliens

---------
-- Game state effect needs access to a random number generator

GS : (Type -> Type) -> Type -> Type
GS m t = Eff m [Gamestate ::: STATE Gamestate, RND] t

moveBullets : Gamestate -> Gamestate
moveBullets gs = let bullets' = movebs (bullets gs) in
                     record { bullets = bullets' } gs
    where movebs [] = []
          movebs ((x, y) :: bs) 
                 = if y < 0 then movebs bs 
                            else ((x, y-5) :: movebs bs)

moveBombs : Gamestate -> Gamestate
moveBombs gs = let bombs' = movebs (bombs gs) in
                   record { bombs = bombs' } gs
    where movebs [] = []
          movebs ((x, y) :: bs) 
                 = if y > 480 then movebs bs 
                              else ((x, y+5) :: movebs bs)

moveAliens : Gamestate -> Gamestate
moveAliens gs = record { aliens = move (bullets gs) (aliens gs) } gs

removeHit : Gamestate -> Gamestate
removeHit gs = let bs = bullets gs in
               let as = aliens gs in
               let (bs', as') = checkHit bs as in
               record { bullets = bs', aliens = as' } gs

drawBullets : List (Int, Int) -> Eff IO [SDL_ON] ()
drawBullets [] = return ()
drawBullets ((x, y) :: bs) = do rectangle red (x-1) (y-4) 2 8
                                drawBullets bs

drawBombs : List (Int, Int) -> Eff IO [SDL_ON] ()
drawBombs [] = return ()
drawBombs ((x, y) :: bs) = do rectangle yellow (x-1) (y-4) 2 8
                              drawBombs bs

randomDropBomb : GS m ()
randomDropBomb = randomDrop (map (Alien.position) (aliens !(Gamestate :- get))) 
 where
   randomDrop : List (Int, Int) -> GS m ()
   randomDrop [] = return ()
   randomDrop ((x, y) :: as) 
        = do if (!(rndInt 1 3000) == 100)  
                 then (do s <- Gamestate :- get
                          let bs = bombs s
                          Gamestate :- put (record { bombs = (x, y+10) :: bs } s))
                 else randomDrop as

---------
updateGamestate : GS m ()
updateGamestate = do gs <- Gamestate :- get
                     let (x, y) = Gamestate.position gs
                     let (x', y') = (bounds 10 630 (x + xmovement gs), 
                                     bounds 380 460 (y + ymovement gs))
                     
                     let gs' = record { position = (x', y') } gs
                     let gs'' = moveAliens (moveBombs (moveBullets gs'))
                     Gamestate :- put (removeHit gs'')
                     randomDropBomb

    where bounds : Int -> Int -> Int -> Int
          bounds low high v = if v < low then low 
                                 else if v > high then high
                                      else v

getPos : GS m (Int, Int)
getPos = do s <- Gamestate :- get
            return (position s)

xmove : Int -> GS m ()
xmove x = do s <- Gamestate :- get
             Gamestate :- put (record { xmovement = x } s) 

ymove : Int -> GS m ()
ymove x = do s <- Gamestate :- get
             Gamestate :- put (record { ymovement = x } s) 

addBullet : GS m ()
addBullet = do s <- Gamestate :- get
               let bs = bullets s
               (x, y) <- getPos
               Gamestate :- put (record { bullets = (x, y-10) :: bs } s)

-- Deal with keypresses from SDL
process : Maybe Event -> GS m Bool
process (Just AppQuit) = return False
process (Just (KeyDown KeyLeftArrow))  = do xmove (-2); return True
process (Just (KeyUp KeyLeftArrow))    = do xmove 0; return True
process (Just (KeyDown KeyRightArrow)) = do xmove 2; return True
process (Just (KeyUp KeyRightArrow))   = do xmove 0; return True
process (Just (KeyDown KeyUpArrow))    = do ymove (-2); return True
process (Just (KeyUp KeyUpArrow))      = do ymove 0; return True
process (Just (KeyDown KeyDownArrow))  = do ymove 2; return True
process (Just (KeyUp KeyDownArrow))    = do ymove 0; return True
process (Just (KeyDown KeySpace))      = do addBullet; return True 
process _ = return True
