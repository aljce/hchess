{-# LANGUAGE DeriveGeneric #-}
module MagicGeneration where

import Prelude hiding (foldr, takeWhile, break, head, (++), null, replicate)
import qualified Prelude as P

import Data.Maybe
import Control.Monad hiding (zipWithM)

import Data.Vector hiding (length)
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U
import qualified Data.Set as S

import Data.Bits
import Data.Word

import System.Random
import Control.Monad.Random
import Control.Monad.Trans

import GHC.Generics
import Data.Aeson
import qualified Data.ByteString.Lazy as B

import Masks
import Move.Tables
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
testMagic occ attSet size mag = ifoldM addOcc (replicate (2^size) Nothing) occ
  where addOcc v i o = addAttSet (v ! index)
          where index = fromIntegral $ (o * mag) `shiftR` (64 - size)
                addAttSet (Just as)
                  | as == attSet ! i = Just v
                  | otherwise        = Nothing
                addAttSet Nothing = Just (v // [(index,Just (attSet ! i))])
                --safeIndex v1 i1 name = maybe (error $ "Name: " P.++ name P.++ "Size:" P.++ show (length v1) P.++ "Index: " P.++ show i1) id (v1 !? i1)

data Magics = Magics {
  magic :: !Word64,
  attackSets :: !(Vector Word64),
  magicShift :: Int,
  attSetLength :: Int} |
  MagicError deriving (Generic, Eq, Show)

instance ToJSON Magics where

instance FromJSON Magics where

genMagics :: (Index -> Vector (Vector Index)) -> Int -> Vector Word64 -> Int -> Magics -> Int -> IO Magics
genMagics attackTransform pieceLoc occ size m@(Magics oldMagic _ _ oldMagicSize) combinations =
  getStdGen >>= checkCombs
  where randMagics :: Int -> RandT StdGen IO Magics
        randMagics i = do
          randMagic <- (\w1 w2 w3 -> w1 .&. w2 .&. w3) <$> getRandom <*> getRandom <*> getRandom
          case testMagic occ attSets size randMagic of
            Just v ->
              let shrunkV = (V.reverse . V.dropWhile isNothing . V.reverse) v in
              if V.length shrunkV < oldMagicSize then do
                lift $ putStrLn "Success!" >> ps randMagic 
                return (Magics randMagic (fmap (fromMaybe maxBound) shrunkV) (64 - size) (V.length shrunkV))
              else do
                  lift $ putStrLn "Failed, new magic makes a larger attack set"
                  return m
            Nothing -> do
              when (i `mod` 1000 == 0 && i /= 0) $ lift $ putStrLn $ show i P.++ " possible bitmaps attempted"
              if i > 100000 then return m else randMagics (i+1)
        attSets = attackSet attackTransform pieceLoc <$> occ 
        checkCombs g
          | oldMagicSize <= combinations = do
              putStrLn "Magic Optimal!"
              return m
          | otherwise = evalRandT (randMagics 0) g
genMagics attackTransform pieceLoc occ size MagicError _ = getStdGen >>= evalRandT (randMagics 0)
  where randMagics :: Int -> RandT StdGen IO Magics
        randMagics i = do
          randMagic <- (\w1 w2 w3 -> w1 .&. w2 .&. w3) <$> getRandom <*> getRandom <*> getRandom
          case testMagic occ attSets size randMagic of
            Just v -> do
              let v' = (V.reverse . V.dropWhile isNothing . V.reverse) v
              lift $ putStrLn "Success!" >> ps randMagic
              return (Magics randMagic (fmap (fromMaybe maxBound) v') (64 - size) (V.length v'))
            Nothing -> do
              when (i `mod` 1000 == 0 && i /= 0) $ lift $ putStrLn $ show i P.++ " possible bitmaps attempted"
              if i > 1000000 then return MagicError else randMagics (i+1)
        attSets = attackSet attackTransform pieceLoc <$> occ

data AllMagics = AllMagics {
  possibleRookAS :: Vector Int,
  possibleBishopAS :: Vector Int, 
  rookMagics :: Vector Magics,
  bishopMagics :: Vector Magics} deriving (Generic, Show)

instance ToJSON AllMagics where

instance FromJSON AllMagics where

checkMagics :: Maybe AllMagics -> AllMagics
checkMagics = fromMaybe (AllMagics prat pbat e e)
  where prat = imap (\i v -> P.length . V.foldr S.insert S.empty $ fmap (attackSet rookAT i) v)
          (occupancyPerms rookMasks)
        pbat = imap (\i v -> P.length . V.foldr S.insert S.empty $ fmap (attackSet bishopAT i) v)
          (occupancyPerms bishopMasks)
        e = V.replicate 64 MagicError

sequenceMagics :: (Index -> Vector (Vector Index)) -> U.Vector Word64
               -> Vector Magics -> Vector Int -> IO (Vector Magics)
sequenceMagics attackTransform masks mags possibleAS =
  V.sequence (V.izipWith4 (genMagics attackTransform)
              (occupancyPerms masks) (fmap popCount (convert masks)) mags possibleAS)

jsonPath :: FilePath
jsonPath = "/home/kyle/Programming/haskell/hchess/resources/magics.json"

onePass :: AllMagics -> IO AllMagics
onePass checkedMagics = do
  allRookMagics   <- sequenceMagics rookAT rookMasks
    (rookMagics checkedMagics) (possibleRookAS checkedMagics)
  allBishopMagics <- sequenceMagics bishopAT bishopMasks
    (bishopMagics checkedMagics) (possibleBishopAS checkedMagics)
  let ams = AllMagics (possibleRookAS checkedMagics)
                      (possibleBishopAS checkedMagics) allRookMagics allBishopMagics
  B.writeFile jsonPath (encode ams)
  return ams

magicMain :: Int -> IO ()
magicMain n = do
  checkedMagics <- (checkMagics . decode) <$> B.readFile jsonPath
  _ <- (V.foldr (<=<) return $ V.replicate n onePass) checkedMagics
  return ()

data UseableMagics = UseableMagics {
  magics :: !(U.Vector Word64),
  attSets :: !(Vector (U.Vector Word64)),
  shifts :: !(U.Vector Int)
}

loadMagics :: IO (UseableMagics, UseableMagics)
loadMagics = do
  ms <- decode <$> B.readFile jsonPath
  case ms of
    (Just ms') -> splitAllMagics ms'
    Nothing -> fail "No parse of magics.json, regenerate magic numbers"
  where splitAllMagics (AllMagics _ _ rookMags bishopMags) =
          (,) <$> magicsToUseable rookMags <*> magicsToUseable bishopMags
        magicsToUseable :: Vector Magics -> IO UseableMagics
        magicsToUseable v
          | V.any (MagicError ==) v = fail "Some magic numbers uninitialized, generate magics"
          | otherwise = (return . toUseable . V.unzip3 . fmap toTuple) v
        toTuple (Magics mag attSet shft _) = (mag, attSet, shft)
        toTuple _ = error "impossible"
        toUseable :: (Vector Word64, Vector (Vector Word64), Vector Int) -> UseableMagics
        toUseable (magV, attSetV, shiftV) = UseableMagics (convert magV) (fmap convert attSetV) (convert shiftV)
