module MagicGeneration where

import Prelude hiding (foldr, takeWhile, break, head, (++), null, replicate)
import qualified Prelude as P

import Data.Maybe
import Control.Monad

import Data.Vector hiding (length)
import qualified Data.Set as S

import Data.Bits
import Data.Word

import System.Random
import Control.Monad.Random
import Control.Monad.Trans

import Masks
import MoveTables
import Serialize
import Index
import Utils

occupancyPerms :: Attacks -> Vector (Vector Word64)
occupancyPerms = fmap genPerms . convert
  where genPerms :: Word64 -> Vector Word64
        genPerms w = transform <$> enumFromN 0 (2^popCount w)
          where transform :: Word64 -> Word64
                transform num = foldr setBits 0 . indexed . convert . indexedOnly $ w
                  where setBits (numI,wI) word
                          | testBit num numI = setBit word wI
                          | otherwise        = word

attackSet :: (Index -> Vector (Vector Index)) -> Index -> Word64 -> Word64
attackSet f index occ = removePiece $ combine genAllMoves
  where removePiece w = complement (bit index) .&. w
        combine = foldl' (.|.) 0 . fmap (foldl' setBit 0 . toStop)
        addOnEnd v1 v2
          | null v2   = v1
          | otherwise = v1 ++ singleton (head v2)
        toStop v = let (strt,end) = break (testBit occ) v in addOnEnd strt end
        genAllMoves = fmap (fmap fromJust . takeWhile isJust .
                    fmap ((!?) board120 >=> id)) . f $ board64 ! index

testMagic :: Vector Word64 -> Vector Word64 -> Int -> Word64 -> Maybe (Vector (Maybe Word64))
testMagic occ attSet size magic = ifoldM addOcc (replicate (2^size) Nothing) occ
  where addOcc v i o = addAttSet (v ! i)
          where index = fromIntegral $ (o * magic) `shiftR` (64 - size)
                addAttSet (Just as)
                  | as == attSet ! i = Just v
                  | otherwise        = Nothing
                addAttSet Nothing = Just (v // [(index,Just (attSet ! i))])
                --safeIndex v1 i1 name = maybe (error $ "Name: " P.++ name P.++ "Size:" P.++ show (length v1) P.++ "Index: " P.++ show i1) id (v1 !? i1)

genMagics :: (Index -> Vector (Vector Index)) -> Vector Word64 -> Int -> IO (Vector (Maybe Word64))
genMagics attackTransform occ size = getStdGen >>= evalRandT randMagics
  where randMagics :: RandT StdGen IO (Vector (Maybe Word64))
        randMagics = do
          randMagic <- (\w1 w2 w3 -> w1 * w2 * w3) <$> getRandom <*> getRandom <*> getRandom
          case testMagic occ attSets size randMagic of
            Just v -> do
              lift $ putStrLn "Success!" >> ps randMagic
              return v
            Nothing -> do
              lift $ putStrLn "Failed, Trying again"
              randMagics
        attSets = (\(i,o) -> attackSet attackTransform (i `mod` 64) o) <$> indexed occ

test = genMagics rookAT (occupancyPerms rookMasks ! 0) 12 

-- rookOccPerms :: Vector (Vector Word64)
-- rookOccPerms = occupancyPerms rookMasks

-- bishopOccPerms :: Vector (Vector Word64)
-- bishopOccPerms = occupancyPerms bishopMasks
