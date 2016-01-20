module Door

import Effects
import Effect.StdIO

data DoorState = Closed | Open

data DoorInfo : DoorState -> Type where
     DI : DoorInfo s

data Jam = Jammed | OK

data Door : Effect where
      OpenDoor : sig Door Jam (DoorInfo Closed)
                           (\jammed =>
                            DoorInfo (case jammed of
                                           Jammed => Closed
                                           OK => Open))
      CloseDoor : sig Door () (DoorInfo Open) (DoorInfo Closed)
      Knock : sig Door () (DoorInfo Closed)

DOOR : DoorState -> EFFECT
DOOR t = MkEff (DoorInfo t) Door

openDoor : Eff Jam [DOOR Closed]
                   (\jammed => [DOOR (case jammed of
                                        Jammed => Closed
                                        OK => Open)])
openDoor = call OpenDoor

closeDoor : Eff () [DOOR Open] [DOOR Closed]
closeDoor = call CloseDoor

knock : Eff () [DOOR Closed]
knock = call Knock

doorProg : Eff () [STDIO, DOOR Closed]
doorProg = do OK <- openDoor | Jammed => putStrLn "It's stuck!"
              closeDoor
              knock
              OK <- openDoor | Jammed => putStrLn "It's stuck!"
              closeDoor



