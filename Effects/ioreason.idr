module Main

import Effect.StdIO
import Effect.State

name : Handler StdIO e => Eff e [STDIO] ()
name = do putStr "Name? "
          n <- getStr
          putStrLn ("Hello " ++ show n)
 
echo : Handler StdIO e => Eff e [STDIO] ()
echo = do n <- getStr
          putStr (show n)
 
streamName : List String -> ((), List String)
streamName = mkStrFn [()] name

main : IO ()
main = run [()] name

