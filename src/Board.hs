module Board where 

import Data.Bits
import Data.Bool.Extras

import BitBoard 
import FEN
import Index 

data Board = Board {
        bitBoard      :: {-# UNPACK #-} !BitBoard,
        turn          :: !Turn,
        castling      :: {-# UNPACK #-} !Castling,
        enPassant     :: !(Maybe Index),
        halfMoveClock :: {-# UNPACK #-} !HalfMoveClock,
        fullMoveClock :: {-# UNPACK #-} !FullMoveClock,
        wKing         :: {-# UNPACK #-} !Index,
        bKing         :: {-# UNPACK #-} !Index }

instance Show Board where 
        show = show . toFEN

startingBoard :: Board 
startingBoard = fromFEN startingFEN 

--TODO: Two options: 1 We move throwing an error to the parsing function 
--                   2 Change the type sig to FEN -> Maybe Board 
fromFEN :: FEN -> Board 
fromFEN (FEN bb t crs ep hc fc) = Board bb t crs ep hc fc (initKings white) (initKings black)
        where initKings color = bool ((countTrailingZeros . color . kings) bb) 
                                     (error "King not placed in FEN")
                                     (((== 0) . color . kings) bb)

toFEN :: Board -> FEN
toFEN (Board bb t c ep hc fc _ _) = FEN bb t c ep hc fc
