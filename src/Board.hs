module Board where 

import BitBoard 
import MailBox
import FEN

data Board = Board {
        bitBoard :: {-# UNPACK #-} !BitBoard,
        turn :: !Turn,
        castling :: !Castling,
        enPassant :: !(Maybe Index),
        halfMoveClock :: {-# UNPACK #-} !HalfMoveClock,
        fullMoveClock :: {-# UNPACK #-} !FullMoveClock,
        wKing :: {-# UNPACK #-} !Index,
        bKing :: {-# UNPACK #-} !Index
}

fromFEN :: FEN -> Board 
fromFEN (FEN bb _ t c ep hc fc) = Board bb t c ep hc fc 

toFEN :: Board -> FEN
toFEN (Board bb mb t c ep hc fc) = FEN bb mb t c ep hc fc
