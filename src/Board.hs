module Board where 

import Prelude hiding (takeWhile)

import Data.Word
import Data.Attoparsec.ByteString.Char8
import Control.Applicative

data Board = Board {-# UNPACK #-} !Word64 --Black Piece Set 
                   {-# UNPACK #-} !Word64 --White Piece Set
                   {-# UNPACK #-} !Word64 --Pawns
                   {-# UNPACK #-} !Word64 --Rooks 
                   {-# UNPACK #-} !Word64 --Knights 
                   {-# UNPACK #-} !Word64 --Bishops
                   {-# UNPACK #-} !Word64 --Queens 
                   {-# UNPACK #-} !Word64 --Kings
                   deriving (Show)

parseBoard :: Parser Board
parseBoard = takeWhile (/= ' ') *> pure (Board 0 0 0 0 0 0 0 0)
