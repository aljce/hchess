module MoveGeneration where

import Prelude hiding ((++))

import Data.Bits
import Data.Word

import Data.Vector.Unboxed
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as M

import Data.Bool.Extras

import BitBoard
import Board
import FEN
import Index
import MoveTypes
import Masks
import MoveTables

{-# SPECIALIZE INLINE expandBitBoard :: (Word64 -> Int ) -> Word64 -> Vector Int  #-}
{-# SPECIALIZE INLINE expandBitBoard :: (Word64 -> Move) -> Word64 -> Vector Move #-}
{-# SPECIALIZE INLINE expandBitBoard :: (Word64 -> (Int,Word64)) -> Word64 ->
    Vector (Int,Word64) #-}
expandBitBoard :: (Unbox a) => (Word64 -> a) -> Word64 -> Vector a
expandBitBoard f w = create $ M.new (popCount w) >>= go 0 w
        where go i b v
                | b == 0    = pure v
                | otherwise = do
                        M.write v i (f (b .&. negate b))
                        go (i+1) (b .&. (b - 1)) v

serializeBitBoard :: MoveData -> Index -> Word64 -> Vector Move
serializeBitBoard md index = expandBitBoard (Move md index . countTrailingZeros)

indexedBitBoard :: Word64 -> Vector (Int,Word64)
indexedBitBoard = expandBitBoard (\b -> (countTrailingZeros b,b))

indexedOnly :: Word64 -> Vector Index
indexedOnly = expandBitBoard countTrailingZeros

{-
genPromotions :: Index -> Word64 -> Vector Move
genPromotions index b = create $ M.new (popCount b * 4) >>= go 0 b
        where ctz = countTrailingZeros
              go i b v
                | b == 0    = pure v
                | otherwise = do
                        M.write v i     (Promotion (Move index (ctz (b .&. negate b))) Knight)
                        M.write v (i+1) (Promotion (Move index (ctz (b .&. negate b))) Bishop)
                        M.write v (i+2) (Promotion (Move index (ctz (b .&. negate b))) Rook)
                        M.write v (i+3) (Promotion (Move index (ctz (b .&. negate b))) Queen)
                        go (i+4) (b .&. (b - 1)) v
-}

genPromotions :: Index -> Word64 -> Moves
genPromotions _ _ = undefined

gPawnGen :: ((Index,Word64) -> (Index,Word64)) -> MoveData -> Word64 -> Vector Move
gPawnGen movement md = U.concatMap (\(!i,!b) -> serializeBitBoard md i b) .
        U.map movement . indexedBitBoard

gIndexGen :: (Index -> (Index, Word64)) -> MoveData -> Word64 -> Vector Move
gIndexGen movement md = U.concatMap (\(!i,!b) -> serializeBitBoard md i b) .
        U.map movement . indexedOnly

gPawnPushAndAttack :: (Word64 -> Word64) -> Attacks -> Mask -> Word64 -> Vector Move
gPawnPushAndAttack shiftF attackTable mask w = gPawnGen mPush SinglePush w
                                            ++ gIndexGen mAttack PawnA w
        where mPush (i,b) = (i,shiftF b)
              mAttack i = (i,mask .&. attackTable ! i)

gEnPassant :: Maybe Index -> Attacks -> Word64 -> Vector Move
gEnPassant (Just ep) attackTable = gIndexGen movement EnPassantA
        where movement i = (i,bit ep .&. (attackTable ! i))
gEnPassant Nothing _ = const empty

gPawnPromotion :: (Word64 -> Word64) -> Attacks -> Mask -> Word64 -> Moves
gPawnPromotion shiftF attackTable mask =
        U.concatMap (\(!i,!b) -> genPromotions i b) . U.map promo . indexedBitBoard
        where promo (i,b) = (i,shiftF b .|. (mask .&. (attackTable ! i)))

pawnMovement :: Turn -> Maybe Index -> AllColors 'Pawns -> AllColors 'All -> Moves
pawnMovement Black ep (AllColors bp _ _) (AllColors _ wa aa) =
        gPawnPushAndAttack doublePush pawnAttackB wa (bp .&. 0x00FF000000000000) ++
        gPawnPushAndAttack singlePush pawnAttackB wa (bp .&. 0x0000FFFFFFFF0000) ++
        gEnPassant ep pawnAttackB (bp .&. 0x00000000FF000000) ++
        gPawnPromotion     singlePush pawnAttackB wa (bp .&. 0x000000000000FF00)
        where singlePush w = (w `shiftR` 8) .&. complement aa
              doublePush w = let sp = singlePush w in sp .|. singlePush sp
pawnMovement White ep (AllColors _ wp _) (AllColors ba _ aa) =
        gPawnPushAndAttack doublePush pawnAttackW ba (wp .&. 0x000000000000FF00) ++
        gPawnPushAndAttack singlePush pawnAttackW ba (wp .&. 0x0000FFFFFFFF0000) ++
        gEnPassant ep pawnAttackW (wp .&. 0x000000FF00000000) ++
        gPawnPromotion     singlePush pawnAttackW ba (wp .&. 0x00FF000000000000)
         where singlePush w = (w `shiftL` 8) .&. complement aa
               doublePush w = let sp = singlePush w in sp .|. singlePush sp
pawnMovement _ _ _ _ =
knightMovement :: Turn -> AllColors Knights -> AllColors All -> Vector Move
knightMovement Black (AllColors bn _ _) (AllColors ba _ _) = gIndexGen movement KnightM bn
        where movement i = (i,complement ba .&. knightAttack ! i)
knightMovement White (AllColors _ wn _) (AllColors _ wa _) = gIndexGen movement KnightM wn
        where movement i = (i,complement wa .&. knightAttack ! i)

bishopMovement :: Turn -> AllColors Bishops -> AllColors All -> Vector Move
bishopMovement t (AllColors bb wb _) (AllColors b w _) = empty

rookMovement :: Turn -> AllColors Rooks -> AllColors All -> Vector Move
rookMovement t (AllColors br wr _) (AllColors b w _) = empty

queenMovement  :: Turn -> AllColors Queens -> AllColors All -> Vector Move
queenMovement t (AllColors bq wq _) (AllColors b w _) = empty

kingMovement :: Turn -> Castling -> Index -> Index -> AllColors All -> Vector Move
kingMovement Black (Castling _ _ ksb qsb) _ bk (AllColors ba _ _) = kingMoves
        where kingMoves = bool (castling ++ noCastling) noCastling inCheck
              noCastling = serializeBitBoard KingM bk $ complement ba .&. kingAttack ! bk
              castling = bool empty empty ksb
              inCheck = False
kingMovement White crs wk _ (AllColors _ wa _) = bool (castling ++ noCastling) noCastling inCheck
        where noCastling = serializeBitBoard KingM wk $ complement wa .&. kingAttack ! wk
              castling = empty
              inCheck = False

inCheck :: Board -> Move -> Bool
inCheck _ _ = False

generateMoves :: Board -> Moves
generateMoves b@(Board (BitBoard pieces pawns rooks
                        knights bishops queens kings) t crs ep _ _ wk bk) =
                        U.filter (inCheck b) moves
        where moves = pawnMovement t ep pawns pieces ++
                      knightMovement t knights pieces ++
                      bishopMovement t bishops pieces ++
                      rookMovement t rooks pieces ++
                      queenMovement t queens pieces ++
                      kingMovement t crs wk bk pieces
