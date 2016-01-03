module MoveGeneration where 

import Prelude hiding ((++))

import Data.Bits
import Data.Word

import Data.Vector.Unboxed 
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as M

import Control.Monad.ST

import BitBoard 
import Board 
import FEN
import Index 
import MoveTypes
import Masks 
import MoveTables

{-# SPECIALIZE INLINE expandBitBoard :: (Word64 -> Word64) -> Word64 -> Vector Word64 #-}
{-# SPECIALIZE INLINE expandBitBoard :: (Word64 -> Move  ) -> Word64 -> Vector Move   #-}
{-# SPECIALIZE INLINE expandBitBoard :: (Word64 -> (Int,Word64)) -> Word64 -> 
    Vector (Int,Word64) #-}

expandBitBoard :: (Unbox a) => (Word64 -> a) -> Word64 -> Vector a
expandBitBoard f b = create $ M.new (popCount b) >>= go 0 b 
        where go i b v
                | b == 0    = pure v
                | otherwise = do
                        M.write v i (f (b .&. negate b))
                        go (i+1) (b .&. (b - 1)) v

serializeBitBoard :: Index -> Word64 -> Vector Move  
serializeBitBoard index = expandBitBoard (Move index . countTrailingZeros) 

populateVector :: Word64 -> Vector (Int,Word64)
populateVector = expandBitBoard (\b -> (countTrailingZeros b,b)) 

genPromotions :: Index -> Word64 -> Vector Promotion
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

gAttackGen :: ((Index,Word64) -> (Index,Word64)) -> Word64 -> Vector Move 
gAttackGen movement = U.concatMap (\(!i,!b) -> serializeBitBoard i b) . 
        U.map movement . populateVector 

gPawnPushAndAttack :: (Word64 -> Word64) -> Attacks -> Mask -> Word64 -> Vector Move 
gPawnPushAndAttack shiftF attackTable mask = gAttackGen movement 
        where movement (i,b) = (i,shiftF b .|. (mask .&. (attackTable ! i)))

gEnPassant :: Maybe Index -> Attacks -> Word64 -> Vector Move 
gEnPassant (Just ep) attackTable = gAttackGen movement 
        where movement (i,b) = (i,bit ep .&. (attackTable ! i))
gEnPassant Nothing _ = const empty 

gPawnPromotion :: (Word64 -> Word64) -> Attacks -> Mask -> Word64 -> Vector Promotion 
gPawnPromotion shiftF attackTable mask = 
        U.concatMap (\(!i,!b) -> genPromotions i b) . U.map promo . populateVector  
        where promo (i,b) = (i,shiftF b .|. (mask .&. (attackTable ! i)))

pawnMovement :: Turn -> Maybe Index -> AllColors Pawns -> AllColors All -> 
                (Vector Move, Vector Promotion)
pawnMovement Black ep (AllColors bp wp _) (AllColors ba wa aa) = 
        (gPawnPushAndAttack doublePush pawnAttackB wa (bp .&. 0x00FF000000000000) ++ 
         gPawnPushAndAttack singlePush pawnAttackB wa (bp .&. 0x0000FFFFFFFF0000) ++ 
         gEnPassant ep pawnAttackB (bp .&. 0x00000000FF000000),
         gPawnPromotion     singlePush pawnAttackB wa (bp .&. 0x000000000000FF00))
         where singlePush w = (w `shiftR` 8) .&. complement aa
               doublePush w = let sp = singlePush w in sp .|. singlePush sp 
pawnMovement White ep (AllColors bp wp _) (AllColors ba wa aa) = 
        (gPawnPushAndAttack doublePush pawnAttackW ba (wp .&. 0x000000000000FF00) ++ 
         gPawnPushAndAttack singlePush pawnAttackW ba (wp .&. 0x0000FFFFFFFF0000) ++ 
         gEnPassant ep pawnAttackW (wp .&. 0x000000FF00000000),
         gPawnPromotion     singlePush pawnAttackW ba (wp .&. 0x00FF000000000000))
         where singlePush w = (w `shiftL` 8) .&. complement aa
               doublePush w = let sp = singlePush w in sp .|. singlePush sp 

knightMovement :: Turn -> AllColors Knights -> AllColors All -> Vector Move 
knightMovement Black (AllColors bn _ _) (AllColors ba _ _) = gAttackGen movement bn 
        where movement (i,b) = (i,complement ba .&. knightAttack ! i)
knightMovement White (AllColors _ wn _) (AllColors _ wa _) = gAttackGen movement wn 
        where movement (i,b) = (i,complement wa .&. knightAttack ! i)

bishopMovement :: Turn -> AllColors Bishops -> AllColors All -> Vector Move 
bishopMovement t (AllColors bb wb _) (AllColors b w _) = empty 

rookMovement :: Turn -> AllColors Rooks -> AllColors All -> Vector Move
rookMovement t (AllColors br wr _) (AllColors b w _) = empty   

queenMovement  :: Turn -> AllColors Queens -> AllColors All -> Vector Move 
queenMovement t (AllColors bq wq _) (AllColors b w _) = empty

kingMovement :: Turn -> Index -> Index -> AllColors Kings -> AllColors All -> Vector Move 
kingMovement t wk bk _ (AllColors b w _) = empty 

generateMoves :: Board -> Moves 
generateMoves (Board (BitBoard pieces pawns rooks 
                      knights bishops queens kings) t crs ep _ _ wk bk) = Moves moves promotions 
        where moves = pawnMoves ++ knightMovement t knights pieces ++ 
                      bishopMovement t bishops pieces ++ 
                      rookMovement t rooks pieces ++
                      queenMovement t queens pieces ++
                      kingMovement t wk bk kings pieces 
              (pawnMoves, promotions) = pawnMovement t ep pawns pieces 
