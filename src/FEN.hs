module FEN where 

import Prelude hiding (takeWhile,take)

import qualified Data.Char as C
import Data.Attoparsec.ByteString.Char8
import Control.Applicative

import BitBoard 
import Data.Bits
import Data.Word

import MailBox
import qualified Data.Vector.Unboxed as U

type Turn = Bool --Should this be a Word8 so it can be unpacked?

data Castling = Castling {
        kingSideW  :: !Bool,
        queenSideW :: !Bool,
        kingSideB  :: !Bool,
        queenSideB :: !Bool } deriving (Show) 

data FEN = FEN {
        bitBoard       :: !BitBoard,
        mailBox        :: !MailBox,
        turn           :: !Turn,
        castlingRights :: !Castling,
        enPassant      :: !(Maybe Word64),
        halfMoveClock  :: {-# UNPACK #-} !Int,
        fullMoveClock  :: {-# UNPACK #-} !Int } deriving (Show)

parseFEN :: Parser FEN
parseFEN = do
        board <- simplifyFENBoard 
        turn <- parseTurn
        char ' '
        castlingRights <- parseCastling
        char ' '
        enPassant <- parseEnPassant
        char ' '
        halfMoveClock <- decimal
        char ' '
        fullMoveClock <- decimal
        return (FEN (toBitBoard board) (toMailBox board) turn 
                castlingRights enPassant halfMoveClock fullMoveClock)


parseTurn :: Parser Turn
parseTurn = True <$ char 'w' <|> False <$ char 'b' <?> "w or b are correct turn characters" 

parseCastling = dash <|> noDash 
        where noDash = Castling <$> charToBool 'K' <*> charToBool 'Q' 
                                <*> charToBool 'k' <*> charToBool 'q'
              dash = Castling False False False False <$ char '-'

charToBool :: Char -> Parser Bool
charToBool c = True <$ char c <|> pure False 

parseEnPassant = Nothing <$ char '-' <|> Just 0 <$ take 2  

toBitBoard :: String -> BitBoard 
toBitBoard = snd . foldr (\c (i,b) -> (i+1,charUpdate i b c)) (0,emptyBoard)
        where charUpdate i (BitBoard bc wc p r n b q k) = \case
                'p' -> BitBoard (setBit bc i) wc (setBit p i) r n b q k 
                'r' -> BitBoard (setBit bc i) wc p (setBit r i) n b q k
                'n' -> BitBoard (setBit bc i) wc p r (setBit n i) b q k
                'b' -> BitBoard (setBit bc i) wc p r n (setBit b i) q k
                'q' -> BitBoard (setBit bc i) wc p r n b (setBit q i) k
                'k' -> BitBoard (setBit bc i) wc p r n b q (setBit k i)
                'P' -> BitBoard bc (setBit wc i) (setBit p i) r n b q k
                'R' -> BitBoard bc (setBit wc i) p (setBit r i) n b q k
                'N' -> BitBoard bc (setBit wc i) p r (setBit n i) b q k
                'B' -> BitBoard bc (setBit wc i) p r n (setBit b i) q k
                'Q' -> BitBoard bc (setBit wc i) p r n b (setBit q i) k
                'K' -> BitBoard bc (setBit wc i) p r n b q (setBit k i) 
                ' ' -> BitBoard bc wc p r n b q k

toMailBox :: String -> MailBox
toMailBox _ = U.empty

simplifyFENBoard :: Parser String 
simplifyFENBoard = concat <$> manyTill' (pieces <|> blank <|> slash) (char ' ')
        where pieces = (:[]) <$> satisfy (\c -> ('a' <= c && c <= 'z') || ('A' <= c && c <= 'Z')) 
              blank = (flip replicate ' ' . subtract 48 . fromEnum) <$> satisfy isDigit
              slash = [] <$ char '/'


