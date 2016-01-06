module Utils where

import qualified Data.Vector.Unboxed as U

import Data.Bits

import MoveGeneration
import Board
import MoveTypes

testingMain = print $ U.length $ generateMoves startingBoard

showWord64 :: (FiniteBits a) => a -> String
showWord64 w = reverse $ fmap (\i -> if testBit w i then '1' else '0') [0..(finiteBitSize w - 1)]

put8 :: String -> IO ()
put8 [] = return ()
put8 s = do
        putStrLn $ reverse $ take 8 s
        put8 $ Prelude.drop 8 s

ps :: (FiniteBits a) => a -> IO ()
ps = put8 . showWord64
