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

type Turn = Bool --Should this be a Word8 so it can be unpacked?

data Castling = Castling {
        kingSideW  :: Bool,
        queenSideW :: Bool,
        kingSideB  :: Bool,
        queenSideB :: Bool
} deriving (Show)

data FEN = FEN {
        board          :: Board,
        turn           :: Turn,
        castlingRights :: Castling,
        enPassant      :: Maybe Word64,
        halfMoveClock  :: Int,
        fullMoveClock  :: Int
} deriving (Show)

parseFEN :: Parser FEN
parseFEN = do
        board <- parseBoard 
        char ' '
        turn <- parseTurn
        char ' '
        castlingRights <- parseCastling 
        char ' '
        enPassant <- parseEnPassant
        char ' '
        halfMoveClock <- decimal
        char ' '
        fullMoveClock <- decimal
        return (FEN board turn castlingRights enPassant halfMoveClock fullMoveClock)

parseBoard :: Parser Board
parseBoard = takeWhile (/= ' ') *> pure (Board 0 0 0 0 0 0 0 0)

parseTurn :: Parser Turn
parseTurn = True <$ char 'w' <|> False <$ char 'b' 

parseCastling = dash <|> noDash 
        where noDash = Castling <$> charToBool 'K' <*> charToBool 'Q' 
                                <*> charToBool 'k' <*> charToBool 'q'
              dash = Castling False False False False <$ char '-'

charToBool :: Char -> Parser Bool
charToBool c = True <$ char c <|> pure False 

parseEnPassant = Nothing <$ char '-' <|> pure (Just 0) 
