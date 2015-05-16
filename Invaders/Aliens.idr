module Aliens

import Effect.SDL
import Effects

record Alien where
    constructor MkAlien 
    position : (Int, Int)
    xmovement : Int
    xstep : Int
    ystep : Int

startAliens : List Alien
startAliens = alienRow (-1) 100 ++
              alienRow 1 150 ++ alienRow (-1) 200
  where
    alienRow : Int -> Int -> List Alien
    alienRow mv y = map (\x => MkAlien (x*60, y) mv 30 15) [1..10]

move : List (Int, Int) -> -- bullet position
       List Alien -> List Alien
move bs as = map moveAlien as
  where moveAlien a = let xstep' = if (xstep a == 0) then 30 else xstep a - 1 in
                      let ystep' = if (ystep a == 0) then 15 else ystep a - 1 in
                      let (x, y) = position a in
                      let y' = if (ystep a == 0) then y+1 else y in
                      let xmovement' = if xstep a == 0 then -(xmovement a)
                                                       else xmovement a in
                      record { position = (x + xmovement a, y'),
                               xmovement = xmovement',
                               xstep = xstep',
                               ystep = ystep' } a

-- if any pair of bullet/alien collide, remove both
checkHit : List (Int, Int) -> List Alien -> (List (Int, Int), List Alien)
checkHit bs as = checkAll bs as [] []
  where testHit : (Int, Int) -> Alien -> Bool
        testHit (x, y) a
            = let (ax, ay) = position a in
                  (abs (x - ax) < 20 && abs (y - ay) < 15) -- hit!

        checkAll : List (Int, Int) -> List Alien -> List Alien -> List (Int, Int) ->
                   (List (Int, Int), List Alien)
        checkAll (b :: bs) (a :: as) asAcc bsAcc
            = if testHit b a 
                 then checkAll bs as asAcc bsAcc
                 else checkAll (b :: bs) as (a :: asAcc) bsAcc
        checkAll (b :: bs) [] asAcc bsAcc
            = checkAll bs asAcc [] (b :: bsAcc)
        checkAll [] as asAcc bsAcc = (bsAcc, as ++ asAcc)
             
drawAliens : List Alien -> { [SDL_ON] } Eff ()
drawAliens [] = return ()
drawAliens (a :: as) = do let (x, y) = Alien.position a
                          drawAlien x y
                          drawAliens as
    where drawAlien : Int -> Int -> { [SDL_ON] } Eff ()
          drawAlien x y = do ellipse green x y 20 16
                             ellipse red (x-8) (y-6) 3 3
                             ellipse red (x-8) (y+6) 3 3
                             rectangle red (x-2) (y-3) 16 4
                             rectangle red (x-2) (y+3) 16 4

