module Move.Application where

import Data.Bits
import Data.Word

import BitBoard
import Board
import Move.Types
import FEN
import MagicGeneration
import Index
import Check

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

applyMove :: Move -> Board -> Board
applyMove (Move md start end) b@(Board bb t crs _ hmc fmc wk bk) = splitMove md
  where splitMove SinglePush = b
        splitMove DoublePush = b
        splitMove PawnA      = b
        splitMove EnPassantA = b
        splitMove KnightM    = b
        splitMove BishopM    = b
        splitMove QueenM     = b
        splitMove KingM      = b
        splitMove CastleL    = b
        splitMove CastleR    = b
        splitMove KnightP    = b
        splitMove BishopP    = b
        splitMove RookP      = b
        splitMove QueenP     = b
        splitMove _ = moveOutOfBoundsError
        newTurn = case t of
          Black -> White
          White -> Black
          _ -> turnNonBinaryError
        newhmc = undefined


