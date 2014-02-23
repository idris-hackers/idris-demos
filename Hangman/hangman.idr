module Main

import Effect.StdIO
import VectMissing

-----------------------------------------------------------------------
-- GAME STATE
-----------------------------------------------------------------------


{- First, the game state, HState, where the type specifies how many guesses
are left and how many missing letters there are still to get. -}

data HState : Nat -> Nat -> Type where
     MkH : (word : String) ->
           (guesses : Nat) ->
           (got : List Char) ->
           (missing : Vect m Char) ->
           HState guesses m

instance Default (HState 0 0) where
    default = MkH "" 0 [] []

instance Show (HState guesses m) where
    show (MkH w _ got missing)
         = let w' = pack (map showGot (unpack w)) in
               w' ++ "\n\n" ++ show guesses ++ " guesses left"
      where showGot : Char -> Char
            showGot ' ' = '/'
            showGot c = if ((not (isAlpha c)) || (c `elem` got)) then c else '-'

{- Initialise the state with the missing letters in a word -}

total
letters : String -> List Char
letters x with (strM x)
  letters "" | StrNil = []
  letters (strCons y xs) | (StrCons y xs) 
          = let xs' = assert_total (letters xs) in
                if ((not (isAlpha y)) || (y `elem` xs')) then xs' else y :: xs'

initState : (x : String) -> HState 6 (length (letters x))
initState w = let xs = letters w in
                  MkH w _ [] (fromList (letters w))

-----------------------------------------------------------------------
-- RULES
-----------------------------------------------------------------------

{- Now, the rules of the game, written as an Effect. 
We can think of the rules as giving a protocol that the game player and
the machine must follow for an implementation of the game to make sense.
-}

data Hangman : Effect where

-- Rule:
-- Precondition: we can make a guess if we have one or more guess available 
-- (S g) and one or more letters are still missing (S w)

-- Postcondition: return whether the character was in the word. If so, reduce
-- the number of missing letters, if not, reduce the number of guesses left

     Guess : (x : Char) ->
             { HState (S g) (S w) ==>
               {inword} if inword then HState (S g) w
                                  else HState g (S w) }
                Hangman Bool

-- The 'Won' operation requires that there are no missing letters

     Won  : { HState g 0 ==> HState 0 0 } Hangman ()

-- The 'Lost' operation requires that there are no guesses left

     Lost : { HState 0 w ==> HState 0 0 } Hangman ()

-- Set up a new game, initialised with 6 guesses and the missing letters in
-- the given word. Note that if there are no letters in the word, we won't
-- be able to run 'Guess'!

     NewWord : (w : String) -> 
               { h ==> HState 6 (length (letters w)) } Hangman ()

-- Finally, allow us to get the current game state
     
     Get  : { h } Hangman h

HANGMAN : Nat -> Nat -> EFFECT
HANGMAN g v = MkEff (HState g v) Hangman

-----------------------------------------------------------------------
-- IMPLEMENTATION OF THE RULES
-----------------------------------------------------------------------

{- This effect handler simply updates the game state as necessary for
each operation. 'Guess' is slightly tricky, in that it needs to check
whether the letter is in the word, and branch accordingly (and if it
is in the word, update the vector of missing letters to be the right
length). -}

using (m : Type -> Type)
  instance Handler Hangman m where
    handle (MkH w g got []) Won k = k () (MkH w 0 got [])
    handle (MkH w Z got m) Lost k = k () (MkH w 0 got [])

    handle st Get k = k st st
    handle st (NewWord w) k = k () (initState w)

    handle (MkH w (S g) got m) (Guess x) k =
      case isElem x m of
           Nothing => k False (MkH w _ got m)
           (Just p) => k True (MkH w _ (x :: got) (shrink m p))

-----------------------------------------------------------------------
-- USER INTERFACE 
-----------------------------------------------------------------------

{- Finally, an implementation of the game which reads user input and calls
the operations we defined above when appropriate -}

game : { [HANGMAN (S g) (S w), STDIO] ==> [HANGMAN 0 0, STDIO] } Eff IO ()
game = do putStrLn (show !Get)
          putStr "Enter guess: "
          let guess = trim !getStr
          case choose (not (guess == "")) of
               (Left p) => processGuess (strHead' guess p)
               (Right p) => do putStrLn "Invalid input!"
                               game
  where 
    processGuess : Char -> { [HANGMAN (S g) (S w), STDIO] ==> 
                             [HANGMAN 0 0, STDIO] }
                           Eff IO ()
    processGuess {g} {w} c
      = case !(Main.Guess c) of
             True => do putStrLn "Good guess!"
                        case w of
                             Z => Won 
                             (S k) => game
             False => do putStrLn "No, sorry"
                         case g of
                              Z => Lost
                              (S k) => game

{- It typechecks! Ship it! -}

runGame : { [HANGMAN 0 0, STDIO] } Eff IO ()
runGame = do NewWord "sausage machine"
             game

{- I made a couple of mistakes while writing this. For example, the following 
were caught by the type checker:

* Forgetting to check the 'Won' state before continuing with 'game'
* Accidentally checking the number of missing letters rather than the number
  of guesses when checking if 'Lost' was callable

-}

main : IO ()
main = run runGame

