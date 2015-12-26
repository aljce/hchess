module MoveGeneration where 

import Prelude hiding ((++))

import Data.Word
import Data.Bits

import Data.Vector hiding ()
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as M
import qualified Data.Vector.Unboxed as U

import BitBoard
import MailBox 
import FEN hiding (mailBox)
import Board

import Text.PrettyPrint.ANSI.Leijen

data Move = Move      {-# UNPACK #-} !Index {-# UNPACK #-} !Index  |
            Promotion {-# UNPACK #-} !Index {-# UNPACK #-} !Square | 
            Castle    {-# UNPACK #-} !Index {-# UNPACK #-} !Index 

instance Show Move where 
        show (Move start end) = show (indexToDoc start <> indexToDoc end) 

type Moves = Vector Move 

generateMoves :: Board -> Moves 
generateMoves b = V.filter (kingNotInCheck b) (traverseMb (U.indexed mbNoEmpty)) 
        where traverseMb = V.concatMap (uncurry (makeMoves b)) . V.convert
              mbNoEmpty = U.filter (\(S p) -> p /= 0) (mailBox b)

kingNotInCheck :: Board -> Move -> Bool
kingNotInCheck (Board _ mb False _ _ _ _) (Move start end) = True 
kingNotInCheck (Board _ mb True  _ _ _ _) (Move start end) = True 

makeMoves :: Board -> Index -> Square -> Moves
makeMoves (Board _ mb False crs ep _ _) index (S piece)
        | piece == 1  = bPawnMoves ep  mb index 
        | piece == 2  = bRookMoves     mb index 
        | piece == 3  = bKnightMoves   mb index 
        | piece == 4  = bBishopMoves   mb index 
        | piece == 5  = bQueenMoves    mb index 
        | piece == 6  = bKingMoves crs mb index  
        | otherwise   = V.empty 
makeMoves (Board _ mb True  crs ep _ _) index (S piece)
        | piece == 7  = wPawnMoves ep  mb index
        | piece == 8  = wRookMoves     mb index 
        | piece == 9  = wKnightMoves   mb index 
        | piece == 10 = wBishopMoves   mb index 
        | piece == 11 = wQueenMoves    mb index 
        | piece == 12 = wKingMoves crs mb index 
        | otherwise   = V.empty

type MoveGenFun = MailBox -> Index -> Moves

sqEmpty :: MailBox -> Index -> Bool 
sqEmpty mb = (\(S p) -> p == 0) . (mb U.!) 

bPawnMoves :: Maybe Index -> MoveGenFun
bPawnMoves ep mb index = V.empty

bRookMoves :: MoveGenFun
bRookMoves mb index = V.empty

bKnightMoves :: MoveGenFun 
bKnightMoves mb index = V.empty

bBishopMoves :: MoveGenFun 
bBishopMoves mb index = V.empty

bQueenMoves :: MoveGenFun 
bQueenMoves mb index = V.empty

bKingMoves :: Castling -> MoveGenFun 
bKingMoves crs mb index = V.empty 

test :: Vector Int
test = create $ do 
        v <- M.new 2 
        M.write v 0 (1:: Int)
        return v

wPawnMoves :: Maybe Index -> MoveGenFun
wPawnMoves ep mb index 
        | index < 24 && sqEmpty mb (index + 8) = create $ do
                v <- M.new 1

                M.write v 0 (Move index (index + 8))
                if sqEmpty mb (index + 16)
                        then do
                                M.unsafeGrow v 1
                                M.write v 1 (Move index (index + 16))
                                return v
                        else return v
        | 24 <= index && index < 48 && sqEmpty mb (index + 8) = create $ do
                v <- M.new 1 
                M.write v 0 (Move index (index + 8))
                return v
        | 48 <= index && index < 56 = undefined 
        | otherwise  = V.empty
        where pawnAttack v mb index =
                if isColor False mb (index + 7) then do
                        M.unsafeGrow v 1 
                        M.write v 2 (Move index (index + 7))
                else if isColor False mb (index + 9) then do
                                M.unsafeGrow v 1
                                M.write v 3 (Move index (index + 9))
                     else return ()
                        
wRookMoves :: MoveGenFun
wRookMoves mb index = V.empty

wKnightMoves :: MoveGenFun 
wKnightMoves mb index = V.empty

wBishopMoves :: MoveGenFun 
wBishopMoves mb index = V.empty

wQueenMoves :: MoveGenFun 
wQueenMoves mb index = V.empty

wKingMoves :: Castling -> MoveGenFun 
wKingMoves crs mb index = V.empty 
