module MoveTables where

import Prelude hiding ((++))

import Data.Bits
import Data.Word

import Data.Vector
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U

import Index

type Attacks = U.Vector Word64

board120 :: Vector (Maybe Index)
board120 = V.replicate 21 Nothing ++ middle ++ V.replicate 19 Nothing
        where middle = V.foldr (\v vs -> v ++ V.replicate 2 Nothing ++ vs) empty center
              center = (\n -> Just <$> enumFromN (8*n) 8) <$> enumFromN 0 8

board64 :: Vector Index
board64 = V.concatMap (`enumFromN` 8) $ enumFromStepN 21 10 8

gAttack :: (Index -> Vector Index) -> Attacks
gAttack f = (convert . fmap (V.foldr combine 0 . f)) board64
        where combine :: Index -> Word64 -> Word64
              combine i w = maybe w (setBit w) (board120 ! i)

pawnAttackW :: Attacks
pawnAttackW = gAttack attackTransform
        where attackTransform i
                | i > 28    = fromList [i+9,i+11]
                | otherwise = empty

pawnAttackB :: Attacks
pawnAttackB = gAttack attackTransform
        where attackTransform i
                | i < 91    = fromList [i-9,i-11]
                | otherwise = empty

knightAttack :: Attacks
knightAttack = gAttack attackTransform
        where attackTransform i = fromList [i+8,i+12,i-8,i-12,i+19,i+21,i-19,i-21]

kingAttack :: Attacks
kingAttack = gAttack attackTransform
        where attackTransform i = fromList [i-11,i-10,i-9,i-1,i+1,i+9,i+10,i+11]
