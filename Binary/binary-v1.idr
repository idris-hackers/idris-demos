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

addBit : Bit x -> Bit y -> Bit c -> Binary 2 (c + x + y)

adc : Binary w x -> Binary w y -> Bit c -> Binary (S w) (x + c + y) 













-- adc zero zero carry ?= zero # carry  
-- adc (numx # bx) (numy # by) carry
--      = let (zero # carry0 # lsb) = addBit bx by carry in 
--            ?adcBody











{-
addBit b0 b0 b0 = zero # b0 # b0 
addBit b0 b0 b1 = zero # b0 # b1
addBit b0 b1 b0 = zero # b0 # b1
addBit b0 b1 b1 = zero # b1 # b0
addBit b1 b0 b0 = zero # b0 # b1
addBit b1 b0 b1 = zero # b1 # b0
addBit b1 b1 b0 = zero # b1 # b0
addBit b1 b1 b1 = zero # b1 # b1
-}














{-
adc zero zero carry = zero # carry
adc (numx # bx) (numy # by) carry
     = let (zero # carry0 # lsb) = addBit bx by carry in 
           ?adcBody
-}


