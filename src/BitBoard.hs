module BitBoard (BitBoard(..), 
                 emptyBoard,
                 BlackPieces(..),
                 WhitePieces(..),
                 Pawns(..),
                 Rooks(..),
                 Knights(..),
                 Bishops(..),
                 Queens(..),
                 Kings(..)) where 

import Prelude hiding (takeWhile)

import Data.Word
import Data.Bits

import Data.IntMap.Strict hiding (filter)
import Text.PrettyPrint.ANSI.Leijen

newtype BlackPieces = BC { unBC :: Word64 } deriving (Bits,FiniteBits,Num,Show,Eq,Ord,Enum)

newtype WhitePieces = WC { unWC :: Word64 } deriving (Bits,FiniteBits,Num,Show,Eq,Ord,Enum)

newtype Pawns   = P { unP :: Word64 } deriving (Bits,FiniteBits,Num,Show,Eq,Ord,Enum)

newtype Rooks   = R { unR :: Word64 } deriving (Bits,FiniteBits,Num,Show,Eq,Ord,Enum)

newtype Knights = N { unN :: Word64 } deriving (Bits,FiniteBits,Num,Show,Eq,Ord,Enum)

newtype Bishops = B { unB :: Word64 } deriving (Bits,FiniteBits,Num,Show,Eq,Ord,Enum)

newtype Queens  = Q { unQ :: Word64 } deriving (Bits,FiniteBits,Num,Show,Eq,Ord,Enum)

newtype Kings   = K { unK :: Word64 } deriving (Bits,FiniteBits,Num,Show,Eq,Ord,Enum)

data BitBoard = BitBoard {-# UNPACK #-} !BlackPieces 
                   {-# UNPACK #-} !WhitePieces 
                   {-# UNPACK #-} !Pawns 
                   {-# UNPACK #-} !Rooks  
                   {-# UNPACK #-} !Knights  
                   {-# UNPACK #-} !Bishops
                   {-# UNPACK #-} !Queens 
                   {-# UNPACK #-} !Kings

emptyBoard :: BitBoard  
emptyBoard = BitBoard 0 0 0 0 0 0 0 0   
 
instance Show BitBoard where 
        show = showBitBoard 
        showsPrec _ b = (showBitBoard b ++) 

showBitBoard :: BitBoard -> String  
showBitBoard (BitBoard bc wc p r n b q k) = (reverse . elems . unions . fmap toIntMap) bitBoards
        where toIntMap :: (Word64,Word64,Char) -> IntMap Char 
              toIntMap (mask,pieces,c) = 
                (fromDistinctAscList . fmap (,c) . filter (testBit (mask .&. pieces))) [0..63] 
              bitBoards = [(unBC bc,unP p,'p'),(unBC bc,unR r,'r'),(unBC bc,unN n,'n'),
                           (unBC bc,unB b,'b'),(unBC bc,unQ q,'q'),(unBC bc,unK k,'k'),
                           (unWC wc,unP p,'P'),(unWC wc,unR r,'R'),(unWC wc,unN n,'N'),
                           (unWC wc,unB b,'B'),(unWC wc,unQ q,'Q'),(unWC wc,unK k,'K'),
                           (-1,-1,' ')]

bitBoardToDoc :: BitBoard -> [Doc]
bitBoardToDoc = fmap string . splitEvery8 0 . showBitBoard 
        where splitEvery8 64 _ = []
              splitEvery8 n xs = take 8 xs : splitEvery8 (n+8) (drop 8 xs)
