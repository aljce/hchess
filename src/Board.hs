module Board where 

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

startingBoard :: Board 
startingBoard = fromFEN startingFEN 

fromFEN :: FEN -> Board 
fromFEN (FEN bb t crs ep hc fc) = Board bb t crs ep hc fc 0 0

toFEN :: Board -> FEN
toFEN (Board bb t c ep hc fc _ _) = FEN bb t c ep hc fc
