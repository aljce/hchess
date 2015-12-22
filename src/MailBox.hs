module MailBox where 

import BitBoard 
import Data.Word

import Data.Vector.Unboxed hiding ()
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as M
import Data.Vector.Unboxed.Deriving

newtype Piece = Piece { unPiece :: Word8 } 

type MailBox = Vector Piece

derivingUnbox "Piece"
        [t| Piece -> Word8 |]
        [| unPiece |]
        [| Piece   |]

mailBoxToBitBoard :: MailBox -> BitBoard 
mailBoxToBitBoard = undefined 

bitBoardToMailBox :: BitBoard -> MailBox 
bitBoardToMailBox (BitBoard bc wc p r n b q k) = undefined 


