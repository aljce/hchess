module MoveTables where

import Data.Bits
import Data.Word 

import Data.Vector.Unboxed
import qualified Data.Vector.Unboxed as U 

type Attacks = Vector Word64 

pawnAttackW :: Attacks
pawnAttackW = U.replicate 64 0

pawnAttackB :: Attacks
pawnAttackB = U.replicate 64 0 

knightAttackTable :: Attacks
knightAttackTable = U.concatMap indexToMoves $ enumFromN 0 63
        where indexToMoves :: Word64 -> Vector Word64 
              indexToMoves n = U.replicate 0 64 // [] 


