{-# LANGUAGE BangPatterns #-}
module Board where 

import Data.IntMap.Strict hiding (map,foldr)
import qualified Data.IntMap.Strict as I
import Data.Attoparsec.Text
import Control.Applicative ((<|>))
import Data.Char (toUpper)

data PieceType = PieceType {-# UNPACK #-} !Int deriving(Eq,Ord,Show)

type Color = Bool

data Piece = Piece !Color {-# UNPACK #-} !PieceType deriving(Eq,Ord,Show) 

allPieceTypes :: [PieceType]
allPieceTypes@[pawn,rook,knight,bishop,queen,king] = fmap PieceType [1..6]

type Board = IntMap Piece

{-# INLINE flattenFileRank #-}
flattenFileRank :: Int -> Int -> Int
flattenFileRank !file !rank = 8*(rank - 1) + file 

startingBoard :: Board
startingBoard = fromDistinctAscList board
        where board = backLine True 1 ++ pawnLine True 2 ++ pawnLine False 7 ++ backLine False 8
              backLine color r = zipWith (\pt f -> (flattenFileRank f r, Piece color pt))
                [rook,knight,bishop,queen,king,bishop,knight,rook] [1..8]
              pawnLine color r = fmap (\f -> (flattenFileRank f r, Piece color pawn)) [1..8]

{-# INLINABLE pieceChar #-}
pieceChar :: Piece -> Char
pieceChar (Piece c pt) = if c then toUpper letter else letter
        where letter
                | pt == pawn = 'p'
                | pt == rook = 'r'
                | pt == knight = 'n'
                | pt == bishop = 'b'
                | pt == queen = 'q'
                | otherwise = 'k'

showBoard :: Board -> String
showBoard board = "*--------*\n" ++ centerText ++ "*--------*\n"
        where centerText = foldr (\line accum -> '|' : line ++ "|\n" ++ accum) [] stringRep 
              stringRep = (reverse . cutEvery8) (snd <$> toList boardWithBlanks)
              cutEvery8 [] = []
              cutEvery8 xs = let (first, second) = splitAt 8 xs in first : cutEvery8 second
              boardWithBlanks = I.map pieceChar board `union` fromDistinctAscList blanks
              blanks = zip [1..64] (repeat ' ')

parsePiece :: Color -> Parser Piece
parsePiece color = Piece color <$> parsePieceType 

parsePieceType :: Parser PieceType 
parsePieceType = letterToPieceType <$> satisfy letters <|> pure pawn
        where letterToPieceType c
                | c == 'N'  = knight
                | c == 'B'  = bishop
                | c == 'R'  = rook
                | c == 'Q'  = queen 
                | otherwise = king 
              letters c = c == 'N' || c == 'B' || c == 'R' || c == 'Q' || c == 'K'

