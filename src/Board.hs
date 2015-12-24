module Board where 

import BitBoard 
import MailBox
import FEN

data Board = Board {
        bitBoard :: BitBoard,
        mailBox :: !MailBox,
        turn :: !Turn,
        castling :: !Castling,
        enPassant :: !(Maybe Index),
        halfMoveClock :: {-# UNPACK #-} !HalfMoveClock,
        fullMoveClock :: {-# UNPACK #-} !FullMoveClock
}

fromFEN :: FEN -> Board 
fromFEN (FEN bb mb t c ep hc fc) = Board bb mb t c ep hc fc 

toFEN :: Board -> FEN
toFEN (Board bb mb t c ep hc fc) = FEN bb mb t c ep hc fc
