module FEN where 

import Prelude hiding (takeWhile,take)

import qualified Data.Char as C
import Data.Attoparsec.ByteString.Char8 hiding (string)
import Control.Applicative
import Text.PrettyPrint.ANSI.Leijen hiding ((<$>),char,bool)
import qualified Text.PrettyPrint.ANSI.Leijen as T

import BitBoard 
import Data.Bits
import Data.Word

import MailBox
import qualified Data.Vector.Unboxed as U

import Utils 

type Turn = Bool --Should this be a Word8 so it can be unpacked?

data Castling = Castling {
        kingSideW  :: !Bool,
        queenSideW :: !Bool,
        kingSideB  :: !Bool,
        queenSideB :: !Bool } deriving (Show) 

castlingRightsToDoc :: Castling -> Doc
castlingRightsToDoc (Castling ksw qsw ksb qsb) = "Castling Rights: " <> str
        where str = bool noDash "-" (ksw || qsw || ksb || qsb)
              noDash = string (concat [boolToChar 'K' ksw,boolToChar 'Q' qsw,
                                       boolToChar 'k' ksb,boolToChar 'q' qsb])
              boolToChar c True  = [c]
              boolToChar _ False = []

type HalfMoveClock = Int
type FullMoveClock = Int

data FEN = FEN {
        bitBoard       :: !BitBoard,
        mailBox        :: !MailBox,
        turn           :: !Turn,
        castlingRights :: !Castling,
        enPassant      :: !(Maybe Index),
        halfMoveClock  :: {-# UNPACK #-} !HalfMoveClock,
        fullMoveClock  :: {-# UNPACK #-} !FullMoveClock } 

fenToDoc :: FEN -> Doc
fenToDoc (FEN _ m t crs ep hc fc) = vsep fen 
        where fen = [board, turnToDoc t,castlingRightsToDoc crs, 
                     enPassantToDoc ep,"Half Move Clock: " <> int hc,
                     "Full Move Clock: " <> int fc]
              board = vsep $ mailBoxToDoc m 
              turnToDoc False = "Turn: Black"
              turnToDoc True  = "Turn: White"
              enPassantToDoc = maybe "En passant square: -" 
                                     (\i -> "En passant square: " <> indexToDoc i)

instance Show FEN where
        showsPrec _ = displayS . renderPretty 0.4 80 . fenToDoc 

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
toMailBox = U.map fromLetter . U.reverse . U.fromList 

simplifyFENBoard :: Parser String 
simplifyFENBoard = concat <$> manyTill' (pieces <|> blank <|> slash) (char ' ')
        where pieces = (:[]) <$> satisfy (\c -> ('a' <= c && c <= 'z') || ('A' <= c && c <= 'Z')) 
              blank = (flip replicate ' ' . subtract 48 . fromEnum) <$> satisfy isDigit
              slash = [] <$ char '/'

parseTurn :: Parser Turn
parseTurn = True <$ char 'w' <|> False <$ char 'b' <?> "no turn parse" 

parseCastling = dash <|> noDash <?> "no castling rights parse"
        where noDash = Castling <$> charToBool 'K' <*> charToBool 'Q' 
                                <*> charToBool 'k' <*> charToBool 'q'
              dash = Castling False False False False <$ char '-'

charToBool :: Char -> Parser Bool
charToBool c = True <$ char c <|> pure False 

parseEnPassant :: Parser (Maybe Index)
parseEnPassant = Nothing <$ char '-' <|> Just <$> parseEPnoDash  <?> "no enpassant parse"
        where parseEPnoDash = combineFR <$> satisfy (\c -> 'a' <= c && c <= 'h') <*>
                                            satisfy (\c -> '1' <= c && c <= '8')
              combineFR :: Char -> Char -> Index
              combineFR f r = flattenRF (fromEnum r - 49) (fromEnum f - 97)

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

startingFEN :: FEN 
startingFEN = either (error "No startingFEN parse, this is impossible.") id $
                parseOnly parseFEN "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"

