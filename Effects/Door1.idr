module Door

import Effects

data DoorState = Closed | Open
data DoorInfo : DoorState -> Type where
     DI : DoorInfo s

data Door : Effect where
     OpenDoor : { DoorInfo Closed ==> DoorInfo Open } Door ()
     CloseDoor : { DoorInfo Open ==> DoorInfo Closed } Door ()
     Knock : { DoorInfo Closed } Door ()

DOOR : DoorState -> EFFECT
DOOR t = MkEff (DoorInfo t) Door

openDoor : { [DOOR Closed] ==> [DOOR Open] } Eff ()
openDoor = call OpenDoor

closeDoor : { [DOOR Open] ==> [DOOR Closed] } Eff ()
closeDoor = call CloseDoor

knock : { [DOOR Closed] } Eff ()
knock = call Knock

doorProg : { [DOOR Closed] } Eff ()
doorProg = do knock
              openDoor
              closeDoor


