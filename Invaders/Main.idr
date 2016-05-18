module Main

import Effects

import Effect.SDL
import Effect.State
import Effect.StdIO

import Rnd
import Aliens
import Starfield
import Gamestate

data Frames : Type where -- empty type, just for labelling

-------
-- SDL effect is parameterised by an underyling 'surface' resource which
-- only exists when initialised.

-- The program supports SDL, carries state, and supports random number
-- generation and console I/O

Prog : Type -> Type -> Type
Prog i t = { [SDL i, 
              Frames ::: STATE Integer,
              Gamestate ::: STATE Gamestate,
              Starfield ::: STATE (List (Int, Int)),
              RND,
              STDIO] } Eff t

-- Convenient shorthand for initialised SDL
Running : Type -> Type
Running t = Prog SDLSurface t

-------
emain : Prog () ()
emain = do putStrLn "Initialising"
           putStrLn "..."
           initialise 640 480
           putStrLn "Initialised"
           initStarfield [] 200
           eventLoop
           quit
  where 
        draw : Running ()
        draw = do rectangle black 0 0 640 480
                  drawStarfield !(Starfield :- get)
                  gs <- Gamestate :- get
                  drawBullets (bullets gs)
                  drawBombs (bombs gs)
                  drawAliens (aliens gs)
                  p <- getPos 
                  let (x, y) = p
                  rectangle blue (x-10) (y-10) 20 20
                  rectangle blue (x-1) (y-20) 2 10
                  flip

        -- update the world state by moving the ellipse to a new position
        -- and scrolling the starfield. Also print the number of frames
        -- drawn so far every so often.

        updateWorld : Running ()
        updateWorld = do f <- Frames :- get
                         Frames :- put (f + 1)
                         when ((f `mod` 100) == 0) (putStrLn (show f))
                         updateStarfield
                         updateGamestate

        -------
        -- Event loop simply has to draw the current state, update the
        -- state according to how the ellipse is moving, then process
        -- any incoming events

        eventLoop : Running ()
        eventLoop = do draw
                       updateWorld
                       when !(process !poll) eventLoop
        

main : IO ()
main = runInit [(), Frames := 0,
                Gamestate := initState,
                Starfield := List.Nil,
                1234567890,
                ()] emain
