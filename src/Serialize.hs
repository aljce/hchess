module Serialize where

import Prelude hiding (concatMap, filter, (++))

import Data.Bits
import Data.Word

import Data.Vector.Unboxed
import qualified Data.Vector.Unboxed.Mutable as M

import Move.Types
import Index

{-# SPECIALIZE INLINE expandBitBoard :: (Word64 -> Word64) -> Word64 -> Vector Word64 #-}
{-# SPECIALIZE INLINE expandBitBoard :: (Word64 -> Int   ) -> Word64 -> Vector Int    #-}
{-# SPECIALIZE INLINE expandBitBoard :: (Word64 -> Move  ) -> Word64 -> Vector Move   #-}
{-# SPECIALIZE INLINE expandBitBoard :: (Word64 -> (Int,Word64)) -> Word64 -> Vector (Int,Word64) #-}
expandBitBoard :: (Bits a, Num a, Unbox b) => (a -> b) -> a -> Vector b
expandBitBoard f w = create $ M.new (popCount w) >>= go 0 w
        where go i b v
                | b == 0    = pure v
                | otherwise = do
                        M.write v i (f (b .&. negate b))
                        go (i+1) (b .&. (b - 1)) v

serializeBitBoard :: MoveData -> Index -> Word64 -> Vector Move
serializeBitBoard md index = expandBitBoard (Move md index . countTrailingZeros)

serializePromotions :: Index -> Word64 -> Vector Move
serializePromotions index = concatMap allPromos . expandBitBoard countTrailingZeros
  where allPromos dest = create $ do
          v <- M.new 4
          M.write v 0 (Move KnightP index dest)
          M.write v 1 (Move BishopP index dest)
          M.write v 2 (Move RookP   index dest)
          M.write v 3 (Move QueenP  index dest)
          return v

bitBoards :: Word64 -> Vector Word64
bitBoards = expandBitBoard id

indexedOnly :: Word64 -> Vector Index
indexedOnly = expandBitBoard countTrailingZeros

