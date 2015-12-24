{-# LANGUAGE MultiParamTypeClasses, TemplateHaskell, TypeFamilies #-}
module MoveGeneration where 

import Prelude hiding ((++))

import Data.Word
import Data.Bits

import Data.Vector.Unboxed hiding (null)
import qualified Data.Vector.Unboxed as U
import Data.Vector.Unboxed.Deriving

import BitBoard
import MailBox 
import Board

data Move = Move {-# UNPACK #-} !Index 
                 {-# UNPACK #-} !Index

derivingUnbox "Move"
        [t| Move -> (Int,Int) |]
        [| \(Move start end) -> (start,end) |]
        [| \(!start,!end) -> Move start end |]

type Moves = Vector Move 

generateMoves ::  -> Moves 
generateMoves = undefined 
