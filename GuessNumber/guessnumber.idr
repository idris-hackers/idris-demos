module Main

import Effects
import Effect.StdIO
import Effect.Random

data GameState = NotRunning
               | Running Nat

data GameInfo : GameState -> Type where
     Number : (guesses : Nat) ->
              (answer : Int) -> GameInfo (Running (S guesses))
     Lost : (answer : Int) -> GameInfo (Running Z)
     Init : GameInfo NotRunning

data Result = TooLow | Correct | TooHigh

data GuessNumber : Effect where
     GuessNum : Int ->
             sig GuessNumber Result
                 (GameInfo (Running (S k)))
                 (\guess =>
                       GameInfo (case guess of
                                      Correct => NotRunning
                                      _ => Running k)) 
     Quit : sig GuessNumber Int (GameInfo (Running Z)) (GameInfo NotRunning)

GUESS : GameState -> EFFECT
GUESS t = MkEff (GameInfo t) GuessNumber

guess : Int -> { [GUESS (Running (S k))] ==>
                 {guess} [GUESS (case guess of
                                      Correct => NotRunning
                                      _ => Running k)] } Eff Result 
guess n = call $ GuessNum n

quit : Eff Int [GUESS (Running Z)] [GUESS NotRunning]
quit = call Quit

Handler GuessNumber m where
    handle (Number g n) (GuessNum i) k with (compare i n)
      handle (Number (S g) n) (GuessNum i) k | LT = k TooLow (Number g n)
      handle (Number Z n) (GuessNum i) k | LT = k TooLow (Lost n)
      handle (Number g n) (GuessNum i) k | EQ = k Correct Init
      handle (Number Z n) (GuessNum i) k | GT = k TooHigh (Lost n)
      handle (Number (S g) n) (GuessNum i) k | GT = k TooHigh (Number g n)
    handle (Lost n) Quit k = k n Init

game : Eff () [GUESS (Running n), STDIO] [GUESS NotRunning, STDIO]
game {n=Z} = do putStrLn "You lose"
                ans <- quit
                putStrLn $ "The answer was " ++ show ans
game {n=S g}
     = do putStr "Guess: "
          let num = cast (trim !getStr)
          case !(guess num) of
               TooLow => do putStrLn "Too low"
                            game
               Correct => putStrLn "You win!"
               TooHigh => do putStrLn "Too high"
                             game

main : IO ()
main = runInit [Number 5 42, ()] game

