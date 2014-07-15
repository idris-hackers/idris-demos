module Door

import Effects
import Effect.StdIO

data DoorState = Closed | Open

data DoorInfo : DoorState -> Type where
     DI : DoorInfo s

data Jam = Jammed | OK

data Door : Effect where
      OpenDoor : { DoorInfo Closed ==> 
                   {jammed} DoorInfo (case jammed of
                                           Jammed => Closed
                                           OK => Open) } Door Jam
      CloseDoor : { DoorInfo Open ==> DoorInfo Closed } Door ()
      Knock : { DoorInfo Closed } Door ()

DOOR : DoorState -> EFFECT
DOOR t = MkEff (DoorInfo t) Door

openDoor : { [DOOR Closed] ==> 
             {jammed} [DOOR (case jammed of
                                  Jammed => Closed
                                  OK => Open)] } Eff Jam
openDoor = call OpenDoor

closeDoor : { [DOOR Open] ==> [DOOR Closed] } Eff ()
closeDoor = call CloseDoor

knock : { [DOOR Closed] } Eff ()
knock = call Knock

doorProg : { [STDIO, DOOR Closed] } Eff ()
doorProg = do OK <- openDoor | Jammed => putStrLn "It's stuck!"
              closeDoor
              knock
              OK <- openDoor | Jammed => putStrLn "It's stuck!"
              closeDoor



