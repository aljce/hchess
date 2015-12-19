module FEN where 

import Prelude hiding (takeWhile)

import Data.Bits
import Data.Word
import Data.Attoparsec.ByteString.Char8
import Control.Applicative

import Board 

type Turn = Bool --Should this be a Word8 so it can be unpacked?

data Castling = Castling {
        kingSideW  :: Bool,
        queenSideW :: Bool,
        kingSideB  :: Bool,
        queenSideB :: Bool } deriving (Show)

data FEN = FEN {
        board          :: Board,
        turn           :: Turn,
        castlingRights :: Castling,
        enPassant      :: Maybe Word64,
        halfMoveClock  :: Int,
        fullMoveClock  :: Int } deriving (Show)

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

parseTurn :: Parser Turn
parseTurn = True <$ char 'w' <|> False <$ char 'b' 

parseCastling = dash <|> noDash 
        where noDash = Castling <$> charToBool 'K' <*> charToBool 'Q' 
                                <*> charToBool 'k' <*> charToBool 'q'
              dash = Castling False False False False <$ char '-'

charToBool :: Char -> Parser Bool
charToBool c = True <$ char c <|> pure False 

parseEnPassant = Nothing <$ char '-' <|> pure (Just 0) 

charUpdate :: Int -> Board -> Char -> Board 
charUpdate i (Board bc wc p r n b q k) = \case
        'p' -> Board (setBit bc i) wc (setBit p i) r n b q k 
        'r' -> Board (setBit bc i) wc p (setBit r i) n b q k
        'n' -> Board (setBit bc i) wc p r (setBit n i) b q k
        'b' -> Board (setBit bc i) wc p r n (setBit b i) q k
        'q' -> Board (setBit bc i) wc p r n b (setBit q i) k
        'k' -> Board (setBit bc i) wc p r n b q (setBit k i)
        'P' -> Board bc (setBit wc i) (setBit p i) r n b q k
        'R' -> Board bc (setBit wc i) p (setBit r i) n b q k
        'N' -> Board bc (setBit wc i) p r (setBit n i) b q k
        'B' -> Board bc (setBit wc i) p r n (setBit b i) q k
        'Q' -> Board bc (setBit wc i) p r n b (setBit q i) k
        'K' -> Board bc (setBit wc i) p r n b q (setBit k i) 
        ' ' -> Board bc wc p r n b q k


parseBoard :: Parser Board 
parseBoard = snd . toBoard . concat <$> many' (pieces <|> blank <|> slash) 
        where toBoard :: String -> (Int,Board) 
              toBoard = foldr (\c (i,b) -> (i+1,charUpdate i b c)) (0,emptyBoard)
              pieces :: Parser String
              pieces = (:[]) <$> satisfy (\c -> ('a' <= c && c <= 'z') || ('A' <= c && c <= 'Z')) 
              blank :: Parser String
              blank = (flip replicate ' ' . subtract 48 . fromEnum) <$> satisfy isDigit
              slash = [] <$ char '/'

