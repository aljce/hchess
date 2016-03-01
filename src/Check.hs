module Check where

import Data.Vector.Unboxed
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector as V

import Data.Word
import Data.Bits

import BitBoard
import FEN
import Index
import Serialize
import Move.Tables
import MagicGeneration
import Masks
import Move.Types

-- TODO Clean up this code, refactor type signatures 

{-
{-# INLINE updateSet #-}
updateSet :: Word64 -> Word64 -> Word64 -> Word64
updateSet start end w = (w .&. complement start) .|. end

moveCheckTest :: UseableMagics -> UseableMagics -> BitBoard -> Turn
              -> Index -> Index -> Move -> Bool
moveCheckTest rms bms (BitBoard (AllColors ba wa aa) pawns@(AllColors bp wp ap)
  rooks@(AllColors br wr ar) knights@(AllColors bn wn an) bishops@(AllColors bb wb ab)
  queens@(AllColors bq wq aq) kings@(AllColors bk wk ak)) Black wki bki (Move md start end)
    = splitMove md
  where splitMove SinglePush = localCheck (BitBoard newPieces (AllColors (localUpSet bp) wp (localUpSet ap))
                                           rooks knights bishops queens kings)
        splitMove DoublePush = localCheck (BitBoard newPieces (AllColors (localUpSet bp) wp (localUpSet ap))
                                           rooks knights bishops queens kings)
        splitMove PawnA      = localCheck (BitBoard newPieces (AllColors (localUpSet bp) wp (localUpSet ap))
                                           rooks knights bishops queens kings)
        splitMove EnPassantA = localCheck (BitBoard (AllColors (localEPSet ba) wa (localEPSet aa))
                                           (AllColors (localEPSet bp) wp (localEPSet ap)) rooks knights bishops queens kings)
        splitMove KnightM    = localCheck (BitBoard newPieces pawns rooks (AllColors (localUpSet bn) wn (localUpSet an))
                                           bishops queens kings)
        splitMove BishopM    = localCheck (BitBoard newPieces pawns rooks knights
                                           (AllColors (localUpSet bb) wb (localUpSet ab)) queens kings)
        splitMove RookM      = localCheck (BitBoard newPieces pawns (AllColors (localUpSet br) wr (localUpSet ar))
                                           knights bishops queens kings)
        splitMove QueenM     = localCheck (BitBoard newPieces pawns rooks knights bishops
                                           (AllColors (localUpSet bq) wq (localUpSet aq)) kings)
        splitMove KingM      = 0 == (bit end .&. checkSet rms bms (BitBoard newPieces pawns rooks knights bishops queens
                                                 (AllColors (localUpSet bk) wk (localUpSet ak))) White wki end)
        splitMove CastleL    = True
        splitMove CastleR    = True
        splitMove KnightP    = localCheck (BitBoard newPieces newPawns rooks
                                           (AllColors (bn .|. bit end) wn (an .|. bit end)) bishops queens kings)
        splitMove BishopP    = localCheck (BitBoard newPieces newPawns rooks knights
                                           (AllColors (bb .|. bit end) wn (ab .|. bit end)) queens kings)
        splitMove RookP      = localCheck (BitBoard newPieces newPawns (AllColors (br .|. bit end) wr (ar .|. bit end))
                                           knights bishops queens kings)
        splitMove QueenP     = localCheck (BitBoard newPieces newPawns rooks knights bishops
                                           (AllColors (bq .|. bit end) wq (aq .|. bit end)) kings)
        splitMove _          = moveOutOfBoundsError
        newPieces = AllColors (localUpSet ba) wa (localUpSet aa)
        newPawns  = AllColors (bp .&. complement (bit start)) wp (bp .&. complement (bit start))
        localCheck newBb = (checkSet rms bms newBb White wki bki .&. bk) == 0
        localUpSet = updateSet (bit start) (bit end)
        localEPSet = updateSet (bit start .|. bit (end - 8)) (bit end)
moveCheckTest _ _ _ _ _ _ _ = True
-}
checkSet :: UseableMagics -> UseableMagics -> BitBoard -> Turn -> Index -> Index -> Word64
checkSet rookMags bishopMags (BitBoard pieces pawns rooks knights bishops queens _) t wk bk =
  pawnSet t pawns .|.
  knightSet t knights .|.
  bishopSet bishopMags t bishops pieces .|.
  rookSet rookMags t rooks pieces .|.
  queenSet rookMags bishopMags t queens pieces .|.
  kingSet t wk bk

type AttSet piece = Turn -> AllColors piece -> AllColors 'All -> Word64

gIndexSetGen :: (Index -> Word64) -> Word64 -> Word64
gIndexSetGen movement = U.foldr' ((.|.) . movement) 0 . indexedOnly

pawnSet :: Turn -> AllColors 'Pawns -> Word64
pawnSet Black (AllColors bp _ _) = gIndexSetGen (pawnAttackB !) bp
pawnSet White (AllColors _ wp _) = gIndexSetGen (pawnAttackW !) wp
pawnSet _ _ = turnNonBinaryError

knightSet :: Turn -> AllColors 'Knights -> Word64
knightSet Black (AllColors bn _ _) =
  gIndexSetGen (knightAttack !) bn
knightSet White (AllColors _ wn _) =
  gIndexSetGen (knightAttack !) wn
knightSet _ _ = turnNonBinaryError

bishopSet :: UseableMagics -> AttSet 'Bishops
bishopSet (UseableMagics mags attckSets shfts) Black (AllColors bb _ _) (AllColors _ _ aa) =
  gIndexSetGen expand bb
  where expand i = attckSets V.! i ! fromIntegral index
          where index = ((aa .&. bishopMasks ! i) * mags ! i) `shiftR` shfts ! i
bishopSet (UseableMagics mags attckSets shfts) White (AllColors _ wb _) (AllColors _ _ aa) =
  gIndexSetGen expand wb
  where expand i = attckSets V.! i ! fromIntegral index
          where index = ((aa .&. bishopMasks ! i) * mags ! i) `shiftR` shfts ! i
bishopSet _ _ _ _ = turnNonBinaryError

rookSet :: UseableMagics -> AttSet 'Rooks
rookSet (UseableMagics mags attckSets shfts) Black (AllColors br _ _) (AllColors _ _ aa) =
  gIndexSetGen expand br
  where expand i = attckSets V.! i ! fromIntegral index
          where index = ((aa .&. rookMasks ! i) * mags ! i) `shiftR` shfts ! i
rookSet (UseableMagics mags attckSets shfts) White (AllColors _ wr _) (AllColors _ _ aa) =
  gIndexSetGen expand wr
  where expand i = attckSets V.! i ! fromIntegral index
          where index = ((aa .&. rookMasks ! i) * mags ! i) `shiftR` shfts ! i
rookSet _ _ _ _ = turnNonBinaryError

queenSet :: UseableMagics -> UseableMagics -> AttSet 'Queens
queenSet (UseableMagics rMags rAttckSets rShfts) (UseableMagics bMags bAttckSets bShfts)
  Black (AllColors bq _ _) (AllColors _ _ aa) = gIndexSetGen expand bq
  where expand i = rookBitBoard .|. bishopBitBoard
          where rookBitBoard = rAttckSets V.! i ! fromIntegral rIndex
                rIndex = ((aa .&. rookMasks ! i) * rMags ! i) `shiftR` rShfts ! i
                bishopBitBoard = bAttckSets V.! i ! fromIntegral bIndex
                bIndex = ((aa .&. bishopMasks ! i) * bMags ! i) `shiftR` bShfts ! i
queenSet (UseableMagics rMags rAttckSets rShfts) (UseableMagics bMags bAttckSets bShfts)
  White (AllColors _ wq _) (AllColors _ _ aa) = gIndexSetGen expand wq
  where expand i = rookBitBoard .|. bishopBitBoard
          where rookBitBoard = rAttckSets V.! i ! fromIntegral rIndex
                rIndex = ((aa .&. rookMasks ! i) * rMags ! i) `shiftR` rShfts ! i
                bishopBitBoard = bAttckSets V.! i ! fromIntegral bIndex 
                bIndex = ((aa .&. bishopMasks ! i) * bMags ! i) `shiftR` bShfts ! i
queenSet _ _ _ _ _ = turnNonBinaryError

-- DONE Investigate if same color pieces are needed to be filtered from check calculation
-- It does not require to be filtered
kingSet :: Turn -> Index -> Index -> Word64
kingSet Black _ bk = kingAttack ! bk
kingSet White wk _ = kingAttack ! wk
kingSet _ _ _ = turnNonBinaryError
