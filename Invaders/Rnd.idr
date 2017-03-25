module Rnd

import Effects

%access public export

data Random : Effect where
     GetRandom : Random Int Int (\v => Int)

Handler Random m where
     handle seed GetRandom k
              = let seed' = assert_total ((1664525 * seed + 1013904223) `prim__sremInt` (pow 2 32)) in
                    k seed' seed'

RND : EFFECT
RND = MkEff Int Random

rndInt : Int -> Int -> { [RND] } Eff Int
rndInt lower upper = do v <- call GetRandom
                        pure (abs (v `prim__sremInt` (upper - lower)) + lower)


