{-# LANGUAGE TemplateHaskell, MultiParamTypeClasses, TypeFamilies, ViewPatterns #-}
module MoveTypes where 

import Data.Bits
import Data.Word

import Data.Vector.Unboxed 
import Data.Vector.Unboxed.Deriving

import Text.PrettyPrint.ANSI.Leijen

import Data.Coerce

import BitBoard 
import Board 
import Index 

newtype MoveData = MoveData Word8 

pattern SinglePush = MoveData 0 
pattern DoublePush = MoveData 1
pattern PawnA      = MoveData 2
pattern EnPassantA = MoveData 3
pattern KnightM    = MoveData 4
pattern BishopM    = MoveData 5
pattern RookM      = MoveData 6
pattern QueenM     = MoveData 7 
pattern KingM      = MoveData 8
pattern CastleL    = MoveData 9
pattern CastleR    = MoveData 10
pattern KnightP    = MoveData 11
pattern BishopP    = MoveData 12 
pattern RookP      = MoveData 13
pattern QueenP     = MoveData 14

data Move = Move {
        metaData :: {-# UNPACK #-} !MoveData,
        from     :: {-# UNPACK #-} !Index, 
        to       :: {-# UNPACK #-} !Index }

moveToDoc :: Move -> Doc 
moveToDoc (Move (MoveData ((<=10) -> True)) from to) = indexToDoc from <> indexToDoc to
moveToDoc (Move md from to) = indexToDoc from <> indexToDoc to <> "p=" <> mdToDoc md
        where mdToDoc KnightP = "N"
              mdToDoc BishopP = "B"
              mdToDoc RookP   = "R"
              mdToDoc QueenP  = "Q"
              mdToDoc _ = ""

instance Show Move where 
        show = show . moveToDoc

derivingUnbox "Move"
        [t| Move -> (Word8,Int,Int) |] 
        [| \(Move m f t) -> (coerce m,f,t) |]
        [| \(m,f,t) -> Move (coerce m) f t |]

type Moves = Vector Move 
