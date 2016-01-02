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
        pieces  :: {-# UNPACK #-} !(AllColors All), 
        pawns   :: {-# UNPACK #-} !(AllColors Pawns),
        rooks   :: {-# UNPACK #-} !(AllColors Rooks),
        knights :: {-# UNPACK #-} !(AllColors Knights),
        bishops :: {-# UNPACK #-} !(AllColors Bishops),
        queens  :: {-# UNPACK #-} !(AllColors Queens),
        kings   :: {-# UNPACK #-} !(AllColors Kings) }

emptyBoard :: BitBoard  
emptyBoard = BitBoard (AllColors 0 0 0) (AllColors 0 0 0) (AllColors 0 0 0) (AllColors 0 0 0)
                      (AllColors 0 0 0) (AllColors 0 0 0) (AllColors 0 0 0)


instance Show BitBoard where 
        show = show . vsep . bitBoardToDoc 
        showsPrec _ = (++) . show 

bitBoardToDoc :: BitBoard -> [Doc]
bitBoardToDoc (BitBoard _ (AllColors bP wP _) (AllColors bR wR _) (AllColors bN wN _)
                          (AllColors bB wB _) (AllColors bQ wQ _) (AllColors bK wK _)) = docs
        where docs = (fmap (string . reverse) . splitEvery8 0) bitBoardString 
              splitEvery8 8 _  = [] 
              splitEvery8 n xs = take 8 xs : splitEvery8 (n+1) (drop 8 xs)
              bitBoardString = (reverse . elems . unions . fmap toIntMap) bitBoards 
              toIntMap (pieces,c) = 
                (fromDistinctAscList . fmap (,c) . filter (testBit pieces)) [0..63]
              bitBoards = [(bP,'p'),(wP,'P'),(bR,'r'),(wR,'R'),(bN,'n'),(wN,'N'),
                           (bB,'b'),(wB,'B'),(bQ,'q'),(wQ,'Q'),(bK,'k'),(wK,'K'),(-1,' ')] 
              
