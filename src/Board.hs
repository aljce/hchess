{-# LANGUAGE TypeFamilies, RankNTypes #-}
module Board (Board(..), emptyBoard) where 

import Prelude hiding (takeWhile)

import Data.Word
import Data.Bits

import Data.IntMap.Strict hiding (filter)

import Data.Coerce

class Color a where 
        extractColor :: a -> Word64 

instance Color BlackPieces where 
        extractColor = unBC

instance Color WhitePieces where 
        extractColor = unWC

newtype BlackPieces = BC { unBC :: Word64 } deriving (Bits,FiniteBits,Num,Show,Eq,Ord,Enum)

newtype WhitePieces = WC { unWC :: Word64 } deriving (Bits,FiniteBits,Num,Show,Eq,Ord,Enum)

newtype Pawns   = P { unP :: Word64 } deriving (Bits,FiniteBits,Num,Show,Eq,Ord,Enum)

newtype Rooks   = R { unR :: Word64 } deriving (Bits,FiniteBits,Num,Show,Eq,Ord,Enum)

newtype Knights = N { unN :: Word64 } deriving (Bits,FiniteBits,Num,Show,Eq,Ord,Enum)

newtype Bishops = B { unB :: Word64 } deriving (Bits,FiniteBits,Num,Show,Eq,Ord,Enum)

newtype Queens  = Q { unQ :: Word64 } deriving (Bits,FiniteBits,Num,Show,Eq,Ord,Enum)

newtype Kings   = K { unK :: Word64 } deriving (Bits,FiniteBits,Num,Show,Eq,Ord,Enum)

data Board = Board {-# UNPACK #-} !BlackPieces 
                   {-# UNPACK #-} !WhitePieces 
                   {-# UNPACK #-} !Pawns 
                   {-# UNPACK #-} !Rooks  
                   {-# UNPACK #-} !Knights  
                   {-# UNPACK #-} !Bishops
                   {-# UNPACK #-} !Queens 
                   {-# UNPACK #-} !Kings 

emptyBoard :: Board 
emptyBoard = Board 0 0 0 0 0 0 0 0   
 
instance Show Board where 
        showsPrec _ b = (showBoard b ++) 

showBoard :: Board -> String  
showBoard (Board bc wc p r n b q k) = (reverse . elems . unions . fmap toIntMap) bitBoards
        where toIntMap :: (Word64,Word64,Char) -> IntMap Char 
              toIntMap (mask,pieces,c) = 
                (fromDistinctAscList . fmap (,c) . filter (testBit (mask .&. pieces))) [0..63] 
              bitBoards = [(unBC bc,unP p,'p'),(unBC bc,unR r,'r'),(unBC bc,unN n,'n'),
                           (unBC bc,unB b,'b'),(unBC bc,unQ q,'q'),(unBC bc,unK k,'k'),
                           (unWC wc,unP p,'P'),(unWC wc,unR r,'R'),(unWC wc,unN n,'N'),
                           (unWC wc,unB b,'B'),(unWC wc,unQ q,'Q'),(unWC wc,unK k,'K'),
                           (-1,-1,' ')]

showWord64 :: (FiniteBits a) => a -> String 
showWord64 w = reverse $ fmap (\i -> if testBit w i then '1' else '0') [0..(finiteBitSize w - 1)]

put8 :: String -> IO ()
put8 [] = return ()
put8 s = do
        putStrLn $ Prelude.take 8 s
        put8 $ Prelude.drop 8 s

