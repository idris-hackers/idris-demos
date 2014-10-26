module Main

import Parity
import System

data Bit : Nat -> Type where
     b0 : Bit 0
     b1 : Bit 1

instance Show (Bit n) where
     show b0 = "0"
     show b1 = "1"

infixl 5 #

data Binary : (width : Nat) -> (value : Nat) -> Type where
     zero : Binary Z Z
     (#)  : Binary w v -> Bit bit -> Binary (S w) (bit + 2 * v)

instance Show (Binary w k) where
     show zero = ""
     show (bin # bit) = show bin ++ show bit

pattern syntax bitpair [x] [y] = (_ ** (_ ** (x, y, _)))
term    syntax bitpair [x] [y] = (_ ** (_ ** (x, y, Refl)))

addBit : Bit x -> Bit y -> Bit c -> 
          (bx ** (by ** (Bit bx, Bit by, c + x + y = by + 2 * bx)))
addBit b0 b0 b0 = bitpair b0 b0
addBit b0 b0 b1 = bitpair b0 b1 
addBit b0 b1 b0 = bitpair b0 b1
addBit b0 b1 b1 = bitpair b1 b0
addBit b1 b0 b0 = bitpair b0 b1 
addBit b1 b0 b1 = bitpair b1 b0 
addBit b1 b1 b0 = bitpair b1 b0 
addBit b1 b1 b1 = bitpair b1 b1 

adc : Binary w x -> Binary w y -> Bit c -> Binary (S w) (c + x + y) 
adc zero        zero        carry ?= zero # carry
adc (numx # bx) (numy # by) carry
   ?= let (bitpair carry0 lsb) = addBit bx by carry in 
          adc numx numy carry0 # lsb







    



