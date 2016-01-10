{-# LANGUAGE DataKinds, KindSignatures, RankNTypes #-}
module BitBoard  where

import Prelude hiding (takeWhile)

import Data.Word
import Data.Bits

import Data.IntMap.Strict hiding (filter)
import Text.PrettyPrint.ANSI.Leijen

data PieceType = All | Pawns | Rooks | Knights | Bishops | Queens | Kings
               deriving (Eq,Ord,Enum,Show)

data AllColors (p :: PieceType) = AllColors {
        black :: {-# UNPACK #-} !Word64,
        white :: {-# UNPACK #-} !Word64,
        both  :: {-# UNPACK #-} !Word64 }

data BitBoard = BitBoard {
        piecesB  :: {-# UNPACK #-} !(AllColors 'All),
        pawnsB   :: {-# UNPACK #-} !(AllColors 'Pawns),
        rooksB   :: {-# UNPACK #-} !(AllColors 'Rooks),
        knightsB :: {-# UNPACK #-} !(AllColors 'Knights),
        bishopsB :: {-# UNPACK #-} !(AllColors 'Bishops),
        queensB  :: {-# UNPACK #-} !(AllColors 'Queens),
        kingsB   :: {-# UNPACK #-} !(AllColors 'Kings) }

emptyBoard :: BitBoard
emptyBoard = BitBoard (AllColors 0 0 0) (AllColors 0 0 0) (AllColors 0 0 0) (AllColors 0 0 0)
                      (AllColors 0 0 0) (AllColors 0 0 0) (AllColors 0 0 0)


instance Show BitBoard where
        show = show . vsep . bitBoardToDoc
        showsPrec _ = (++) . show

splitEveryN :: Int -> [a] -> [[a]]
splitEveryN delta = unfold 0
  where unfold n xs
          | n < delta * delta = take delta xs : unfold (n + delta) (drop delta xs)
          | otherwise         = []

bitBoardToDoc :: BitBoard -> [Doc]
bitBoardToDoc (BitBoard _ (AllColors bP wP _) (AllColors bR wR _) (AllColors bN wN _)
                          (AllColors bB wB _) (AllColors bQ wQ _) (AllColors bK wK _)) = docs
        where docs = (fmap (string . reverse) . splitEveryN 8) bitBoardString
              bitBoardString = (reverse . elems . unions . fmap toIntMap) bitBoards
              toIntMap (ps,c) =
                (fromDistinctAscList . fmap (,c) . filter (testBit ps)) [0..63]
              bitBoards = [(bP,'p'),(wP,'P'),(bR,'r'),(wR,'R'),(bN,'n'),(wN,'N'),
                           (bB,'b'),(wB,'B'),(bQ,'q'),(wQ,'Q'),(bK,'k'),(wK,'K'),(-1,' ')]

