module MoveGeneration where

import Prelude hiding (concatMap, filter, (++))

import Control.Arrow

import Data.Bits
import Data.Word

import Data.Vector.Unboxed
import qualified Data.Vector.Unboxed.Mutable as M
import qualified Data.Vector as V

import MoveTypes
import Index
import BitBoard
import Board
import FEN
import Masks
import MoveTables
import Serialize
import MagicGeneration

gIndexGen :: (Index -> Word64) -> MoveData -> Word64 -> Moves
gIndexGen movement md = concatMap (\i -> serializeBitBoard md i (movement i)) . indexedOnly

type MoveGen piece = Turn -> AllColors piece -> AllColors 'All -> Moves

gPawnPush :: (Word64 -> Word64) -> MoveData -> Word64 -> Moves
gPawnPush shiftF md = concatMap (\b -> serializeBitBoard md (countTrailingZeros b) (shiftF b)) . bitBoards

gPawnAttack :: Attacks -> Mask -> Word64 -> Moves
gPawnAttack attackTable mask = gIndexGen movement PawnA
  where movement i = mask .&. attackTable ! i

gEnPassant :: Maybe Index -> Attacks -> Word64 -> Vector Move
gEnPassant (Just ep) attackTable = gIndexGen movement EnPassantA
  where movement i = bit ep .&. (attackTable ! i)
gEnPassant Nothing _ = const empty

gPawnPromotion :: (Word64 -> Word64) -> Attacks -> Mask -> Word64 -> Moves
gPawnPromotion shiftF attackTable mask = concatMap promo . bitBoards
  where promo b = serializePromotions (countTrailingZeros b) (shiftF b .|. (mask .&. attackTable ! 1))

pawnMovement :: Maybe Index -> MoveGen 'Pawns
pawnMovement ep Black (AllColors bp _ _) (AllColors _ wa aa) =
  gPawnPush singlePush SinglePush (bp .&. 0x00FFFFFFFFFF0000) ++
  gPawnPush doublePush DoublePush (bp .&. 0x00FF000000000000) ++
  gPawnAttack pawnAttackB wa (bp .&. 0x00FFFFFFFFFF0000) ++
  gPawnPromotion singlePush pawnAttackB wa (bp .&. 0x000000000000FF00) ++
  gEnPassant ep pawnAttackB (bp .&. 0x000000FF000000)
  where singlePush w = (w `shiftR` 8) .&. complement aa
        doublePush   = singlePush . singlePush
pawnMovement ep White (AllColors _ wp _) (AllColors ba _ aa) =
  gPawnPush      singlePush SinglePush (wp .&. 0x0000FFFFFFFFFF00) ++
  gPawnPush      doublePush DoublePush (wp .&. 0x000000000000FF00) ++
  gPawnAttack    pawnAttackW ba (wp .&. 0x0000FFFFFFFFFF00) ++
  gPawnPromotion singlePush pawnAttackW ba (wp .&. 0x00FF000000000000) ++
  gEnPassant     ep pawnAttackW (wp .&. 0x000000FF00000000)
  where singlePush w = (w `shiftL` 8) .&. complement aa
        doublePush   = singlePush . singlePush
pawnMovement _ _ _ _ = turnNonBinaryError

knightMovement :: MoveGen 'Knights
knightMovement Black (AllColors bn _ _) (AllColors ba _ _) = gIndexGen movement KnightM bn
  where movement i = complement ba .&. knightAttack ! i
knightMovement White (AllColors _ wn _) (AllColors _ wa _) = gIndexGen movement KnightM wn
  where movement i = complement wa .&. knightAttack ! i
knightMovement _ _ _ = turnNonBinaryError

gBishopMove :: UseableMagics -> Word64 -> Word64 -> Word64 -> Moves
gBishopMove (UseableMagics mags attckSets shfts) bishops sameColor allPieces =
  concatMap expand (indexedOnly bishops)
  where expand i = serializeBitBoard BishopM i ((attckSets V.! i ! fromIntegral index) .&. complement sameColor)
          where index = ((allPieces .&. bishopMasks ! i) * mags ! i) `shiftR` shfts ! i

bishopMovement :: UseableMagics -> MoveGen 'Bishops
bishopMovement ums Black (AllColors br _ _) (AllColors ba _ aa) = gBishopMove ums br ba aa
bishopMovement ums White (AllColors _ wr _) (AllColors _ wa aa) = gBishopMove ums wr wa aa
bishopMovement _ _ _ _ = turnNonBinaryError

gRookMove :: UseableMagics -> Word64 -> Word64 -> Word64 -> Moves
gRookMove (UseableMagics mags attckSets shfts) rooks sameColor allPieces =
  concatMap expand (indexedOnly rooks)
  where expand i = serializeBitBoard RookM i ((attckSets V.! i ! fromIntegral index) .&. complement sameColor)
          where index = ((allPieces .&. rookMasks ! i) * mags ! i) `shiftR` shfts ! i

rookMovement :: UseableMagics -> MoveGen 'Rooks
rookMovement ums Black (AllColors br _ _) (AllColors ba _ aa) = gRookMove ums br ba aa
rookMovement ums White (AllColors _ wr _) (AllColors _ wa aa) = gRookMove ums wr wa aa
rookMovement _ _ _ _ = turnNonBinaryError

gQueenMove :: UseableMagics -> UseableMagics -> Word64 -> Word64 -> Word64 -> Moves
gQueenMove (UseableMagics rMags rAttckSets rShfts) (UseableMagics bMags bAttckSets bShfts)
  queens sameColor allPieces = concatMap expand (indexedOnly queens)
  where expand i = serializeBitBoard QueenM i (rookBitBoard .|. bishopBitBoard)
          where rookBitBoard = (rAttckSets V.! i ! fromIntegral rIndex) .&. complement sameColor
                rIndex = ((allPieces .&. rookMasks ! i) * rMags ! i) `shiftR` rShfts ! i
                bishopBitBoard = (bAttckSets V.! i ! fromIntegral bIndex) .&. complement sameColor
                bIndex = ((allPieces .&. bishopMasks ! i) * bMags ! i) `shiftR` bShfts ! i

queenMovement :: UseableMagics -> UseableMagics -> MoveGen 'Queens
queenMovement rums bums Black (AllColors bq _ _) (AllColors ba _ aa) = gQueenMove rums bums bq ba aa
queenMovement rums bums White (AllColors _ wq _) (AllColors _ wa aa) = gQueenMove rums bums wq wa aa
queenMovement _ _ _ _ _ = turnNonBinaryError


gKingMove :: Index -> Mask -> Moves
gKingMove k sameColorPieces = serializeBitBoard KingM k (complement sameColorPieces .&. kingAttack ! k)

gCastling :: Bool -> Bool -> Index -> Word64 -> Moves
gCastling _ _ _ _ = empty

kingMovement :: Castling -> Index -> Index -> Turn -> AllColors 'All -> Moves
kingMovement crs _ bk Black (AllColors ba _ aa) = gKingMove bk ba
kingMovement crs wk _ White (AllColors _ wa aa) = gKingMove wk wa
kingMovement _ _ _ _ _ = turnNonBinaryError

notInCheck :: Board -> Move -> Bool
notInCheck _ _ = True

generateMoves :: UseableMagics -> UseableMagics -> Board -> Moves
generateMoves rookMags bishopMags b@(Board (BitBoard pieces pawns rooks knights bishops queens _)
                 t crs ep _ _ wk bk) = filter (notInCheck b) moves
  where moves = pawnMovement ep t pawns pieces ++
                knightMovement t knights pieces ++
                bishopMovement bishopMags t bishops pieces ++
                rookMovement rookMags t rooks pieces ++
                queenMovement rookMags bishopMags t queens pieces ++
                kingMovement crs wk bk t pieces
