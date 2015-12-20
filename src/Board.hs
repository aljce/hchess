module Board (Board(..), 
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

import GHC.TypeLits

newtype BlackPieces (n :: Nat) = BC { unBC :: Word64 } 
        deriving (Bits,FiniteBits,Num,Show,Eq,Ord,Enum)

newtype WhitePieces (n :: Nat) = WC { unWC :: Word64 } 
        deriving (Bits,FiniteBits,Num,Show,Eq,Ord,Enum)

newtype Pawns   (n :: Nat) = P { unP :: Word64 } deriving (Bits,FiniteBits,Num,Show,Eq,Ord,Enum)

newtype Rooks   (n :: Nat) = R { unR :: Word64 } deriving (Bits,FiniteBits,Num,Show,Eq,Ord,Enum)

newtype Knights (n :: Nat) = N { unN :: Word64 } deriving (Bits,FiniteBits,Num,Show,Eq,Ord,Enum)

newtype Bishops (n :: Nat) = B { unB :: Word64 } deriving (Bits,FiniteBits,Num,Show,Eq,Ord,Enum)

newtype Queens  (n :: Nat) = Q { unQ :: Word64 } deriving (Bits,FiniteBits,Num,Show,Eq,Ord,Enum)

newtype Kings   (n :: Nat) = K { unK :: Word64 } deriving (Bits,FiniteBits,Num,Show,Eq,Ord,Enum)

data Board :: (Nat -> Nat -> Nat -> Nat -> Nat -> Nat -> Nat -> Nat -> *) where 
        Board :: BlackPieces nBP -> WhitePieces nWP -> Pawns nP  -> Rooks nR -> 
                 Knights nN      -> Bishops nB      -> Queens nQ -> Kings nK -> 
                 Board nBP nWP nP nR nN nB nQ nK

emptyBoard :: Board 0 0 0 0 0 0 0 0  
emptyBoard =  Board 0 0 0 0 0 0 0 0   
 
instance Show (Board wc bc p r n b q k) where 
        show = showBoard 
        showsPrec _ b = (showBoard b ++) 

--showBoard :: Board -> String  
showBoard (Board bc wc p r n b q k) = (reverse . elems . unions . fmap toIntMap) bitBoards
        where toIntMap :: (Word64,Word64,Char) -> IntMap Char 
              toIntMap (mask,pieces,c) = 
                (fromDistinctAscList . fmap (,c) . filter (testBit (mask .&. pieces))) [0..63] 
              bitBoards = [(unBC bc,unP p,'p'),(unBC bc,unR r,'r'),(unBC bc,unN n,'n'),
                           (unBC bc,unB b,'b'),(unBC bc,unQ q,'q'),(unBC bc,unK k,'k'),
                           (unWC wc,unP p,'P'),(unWC wc,unR r,'R'),(unWC wc,unN n,'N'),
                           (unWC wc,unB b,'B'),(unWC wc,unQ q,'Q'),(unWC wc,unK k,'K'),
                           (-1,-1,' ')]
