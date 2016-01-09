{-# LANGUAGE PatternSynonyms #-}
module FEN where

import Prelude hiding (takeWhile,take)
import qualified Prelude as P

import qualified Data.Char as C
import Data.Attoparsec.ByteString.Char8 hiding (string)
import Control.Applicative
import Text.PrettyPrint.ANSI.Leijen hiding ((<$>),char,bool)
import qualified Text.PrettyPrint.ANSI.Leijen as T
import Data.Bool.Extras

import BitBoard
import Data.Bits
import Data.Word

import Index

data Castling = Castling {
        kingSideW  :: !Bool,
        queenSideW :: !Bool,
        kingSideB  :: !Bool,
        queenSideB :: !Bool } deriving (Show)

castlingRightsToDoc :: Castling -> Doc
castlingRightsToDoc (Castling ksw qsw ksb qsb) = "Castling Rights: " <> str
        where str = bool "-" noDash (ksw || qsw || ksb || qsb)
              noDash = string (concat [boolToChar 'K' ksw,boolToChar 'Q' qsw,
                                       boolToChar 'k' ksb,boolToChar 'q' qsb])
              boolToChar c True  = [c]
              boolToChar _ False = []

newtype Turn = Turn Word8

pattern Black = Turn 0
pattern White = Turn 1

type HalfMoveClock = Int
type FullMoveClock = Int

data FEN = FEN {
        bitBoard       :: !BitBoard,
        turn           :: !Turn,
        castlingRights :: !Castling,
        enPassant      :: !(Maybe Index),
        halfMoveClock  :: {-# UNPACK #-} !HalfMoveClock,
        fullMoveClock  :: {-# UNPACK #-} !FullMoveClock }

fenToDoc :: FEN -> Doc
fenToDoc (FEN bb t crs ep hc fc) = vsep fen
        where fen = [board,
                     "  ABCDEFGH",
                     turnToDoc t,
                     castlingRightsToDoc crs,
                     enPassantToDoc ep,
                     "Half Move Clock: " <> int hc,
                     "Full Move Clock: " <> int fc]
              board = vsep $ zipWith (\r b -> T.char r <> T.space <> b)
                        "87654321" (bitBoardToDoc bb)
              turnToDoc Black = "Turn: Black"
              turnToDoc White = "Turn: White"
              enPassantToDoc = maybe "En passant square: -"
                                     (\i -> "En passant square: " <> indexToDoc i)

instance Show FEN where
        showsPrec _ = displayS . renderPretty 0.4 80 . fenToDoc

toBitBoard :: String -> BitBoard
toBitBoard = merge . snd . foldr (\c (i,b) -> (i+1,charUpdate i b c)) (0,emptyBoard)
        where merge b@(BitBoard _ (AllColors bp wp ap) (AllColors br wr ar) (AllColors bn wn an)
                                  (AllColors bb wb ab) (AllColors bq wq aq) (AllColors bk wk ak)) =
                        b { pieces = AllColors (bp .|. br .|. bn .|. bb .|. bq .|. bk)
                                               (wp .|. wr .|. wn .|. wb .|. wq .|. wk)
                                               (ap .|. ar .|. an .|. ab .|. aq .|. ak) }
              updatePiece False i (AllColors b w a) = AllColors (setBit b i) w (setBit a i)
              updatePiece True  i (AllColors b w a) = AllColors b (setBit w i) (setBit a i)
              charUpdate i board = \case
                'p' -> board { pawns   = updatePiece False i (pawns board) }
                'r' -> board { rooks   = updatePiece False i (rooks board) }
                'n' -> board { knights = updatePiece False i (knights board) }
                'b' -> board { bishops = updatePiece False i (bishops board) }
                'q' -> board { queens  = updatePiece False i (queens board) }
                'k' -> board { kings   = updatePiece False i (kings board) }
                'P' -> board { pawns   = updatePiece True  i (pawns board) }
                'R' -> board { rooks   = updatePiece True  i (rooks board) }
                'N' -> board { knights = updatePiece True  i (knights board) }
                'B' -> board { bishops = updatePiece True  i (bishops board) }
                'Q' -> board { queens  = updatePiece True  i (queens board) }
                'K' -> board { kings   = updatePiece True  i (kings board) }
                _   -> board

simplifyFENBoard :: Parser String
simplifyFENBoard = merge <$> manyTill' (pieces <|> blank <|> slash) (char ' ')
        where merge = concat . fmap reverse . splitEvery8 0 . concat
              splitEvery8 8 _ = []
              splitEvery8 n xs = P.take 8 xs : splitEvery8 (n+1) (P.drop 8 xs)
              pieces = (:[]) <$> satisfy (\c -> ('a' <= c && c <= 'z') || ('A' <= c && c <= 'Z'))
              blank = (flip replicate ' ' . subtract 48 . fromEnum) <$> satisfy isDigit
              slash = [] <$ char '/'

parseTurn :: Parser Turn
parseTurn = White <$ char 'w' <|> Black <$ char 'b' <?> "no turn parse"

parseCastling = dash <|> noDash <?> "no castling rights parse"
        where noDash = Castling <$> charToBool 'K' <*> charToBool 'Q'
                                <*> charToBool 'k' <*> charToBool 'q'
              dash = Castling False False False False <$ char '-'

charToBool :: Char -> Parser Bool
charToBool c = True <$ char c <|> pure False

parseEnPassant :: Parser (Maybe Index)
parseEnPassant = Nothing <$ char '-' <|> Just <$> parseIndex <?> "no enpassant parse"

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
        return (FEN (toBitBoard board) turn castlingRights
                enPassant halfMoveClock fullMoveClock)

startingFEN :: FEN
startingFEN = either (error "No startingFEN parse, this is impossible.") id $
                parseOnly parseFEN "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"

testingFEN = either (error "fuck you") id $
                parseOnly parseFEN "8/ppp1pppp/8/3p4/3P4/8/P7/8 w KQkq - 0 1"
