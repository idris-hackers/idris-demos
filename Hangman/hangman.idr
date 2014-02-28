module Main

import Effect.StdIO
import Effect.System
import Effect.Random
import VectMissing

-----------------------------------------------------------------------
-- GAME STATE
-----------------------------------------------------------------------

{- First, the game state, HState, where the type specifies how many guesses
are left and how many missing letters there are still to get. -}

data HState = Running Nat Nat | NotRunning

data Hangman : HState -> Type where
     Init     : Hangman NotRunning -- initialising, but not ready
     GameWon  : String -> Hangman NotRunning
     GameLost : String -> Hangman NotRunning
     MkH      : (word : String) ->
                (guesses : Nat) ->
                (got : List Char) ->
                (missing : Vect m Char) ->
                Hangman (Running guesses m)

-- Some candidate words. We'll use programming languages.
-- I used \o to infer the length :).

words : Vect 10 String 
words = ["idris","agda","haskell","miranda",
         "java","javascript","fortran","basic",
         "coffeescript","rust"]

instance Default (Hangman NotRunning) where
    default = Init

instance Show (Hangman s) where
    show Init = "Not ready yet"
    show (GameWon w) = "You won! Successfully guessed " ++ w
    show (GameLost w) = "You lost! The word was " ++ w
    show (MkH w guesses got missing)
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

initState : (x : String) -> Hangman (Running 6 (length (letters x)))
initState w = let xs = letters w in
                  MkH w _ [] (fromList (letters w))

-----------------------------------------------------------------------
-- RULES
-----------------------------------------------------------------------

{- Now, the rules of the game, written as an Effect. 
We can think of the rules as giving a protocol that the game player and
the machine must follow for an implementation of the game to make sense.
-}

data HangmanRules : Effect where

-- Rule:
-- Precondition: we can make a guess if we have one or more guess available 
-- (S g) and one or more letters are still missing (S w)

-- Postcondition: return whether the character was in the word. If so, reduce
-- the number of missing letters, if not, reduce the number of guesses left

     Guess : (x : Char) ->
             { Hangman (Running (S g) (S w)) ==>
               {inword} if inword then Hangman (Running (S g) w)
                                  else Hangman (Running g (S w)) }
                HangmanRules Bool

-- The 'Won' operation requires that there are no missing letters

     Won  : { Hangman (Running g 0) ==> Hangman NotRunning } HangmanRules ()

-- The 'Lost' operation requires that there are no guesses left

     Lost : { Hangman (Running 0 g) ==> Hangman NotRunning } HangmanRules ()

-- Set up a new game, initialised with 6 guesses and the missing letters in
-- the given word. Note that if there are no letters in the word, we won't
-- be able to run 'Guess'!

     NewWord : (w : String) -> 
               { h ==> Hangman (Running 6 (length (letters w))) } HangmanRules ()

-- Finally, allow us to get the current game state
     
     Get  : { h } HangmanRules h

HANGMAN : HState -> EFFECT
HANGMAN h = MkEff (Hangman h) HangmanRules

-----------------------------------------------------------------------
-- IMPLEMENTATION OF THE RULES
-----------------------------------------------------------------------

{- This effect handler simply updates the game state as necessary for
each operation. 'Guess' is slightly tricky, in that it needs to check
whether the letter is in the word, and branch accordingly (and if it
is in the word, update the vector of missing letters to be the right
length). -}

using (m : Type -> Type)
  instance Handler HangmanRules m where
    handle (MkH w g got []) Won k = k () (GameWon w)
    handle (MkH w Z got m) Lost k = k () (GameLost w)

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
the operations we defined above when appropriate. 

The type indicates that the game must start in a running state, with some
guesses available, and get to a not running state (i.e. won or lost). 
Since we picked a word at random, we can't actually make the assumption there
were valid letters in it!
-}

game : { [HANGMAN (Running (S g) w), STDIO] ==> 
         [HANGMAN NotRunning, STDIO] } Eff IO ()
game {w=Z} = Won 
game {w=S _}
     = do putStrLn (show !Get)
          putStr "Enter guess: "
          let guess = trim !getStr
          case choose (not (guess == "")) of
               (Left p) => processGuess (strHead' guess p)
               (Right p) => do putStrLn "Invalid input!"
                               game
  where 
    processGuess : Char -> { [HANGMAN (Running (S g) (S w)), STDIO] ==> 
                             [HANGMAN NotRunning, STDIO] }
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

runGame : { [HANGMAN NotRunning, RND, SYSTEM, STDIO] } Eff IO ()
runGame = do srand (cast !time)
             let w = index !(rndFin _) words
             NewWord w
             game
             putStrLn (show !Get)

{- I made a couple of mistakes while writing this. For example, the following 
were caught by the type checker:

* Forgetting to check the 'Won' state before continuing with 'game'
* Accidentally checking the number of missing letters rather than the number
  of guesses when checking if 'Lost' was callable

-}

main : IO ()
main = run runGame

