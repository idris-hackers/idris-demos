module Main

import Effect.StdIO
import Effect.State

instance Show () where
    show () = "()"

name : Handler StdIO e => Eff e [STDIO] ()
name = do putStr "Name? "
          n <- getStr
          putStrLn ("Hello " ++ show n)
 
echo : Handler StdIO e => Eff e [STDIO] ()
echo = do n <- getStr
          putStr (show n)
 
streamName : List String -> ((), List String)
streamName = mkStrFn [()] name

streamEcho : List String -> ((), List String)
streamEcho = mkStrFn [()] echo

echoEchoes : (input : List String) -> streamEcho input = ((), input)
echoEchoes [x] = ?ecase

main : IO ()
main = run [()] name

