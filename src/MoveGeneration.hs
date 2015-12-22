module MoveGeneration where 

import Data.Word
import Data.Bits

import Data.Vector.Unboxed hiding (null)
import qualified Data.Vector.Unboxed as U

import BitBoard

data Move = Move {-# UNPACK #-} !Word64 
                 {-# UNPACK #-} !Word64

generateMoves b = undefined  
