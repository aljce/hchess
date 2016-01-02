module Utils where 

import Data.Bits

showWord64 :: (FiniteBits a) => a -> String 
showWord64 w = reverse $ fmap (\i -> if testBit w i then '1' else '0') [0..(finiteBitSize w - 1)]

put8 :: String -> IO ()
put8 [] = return ()
put8 s = do
        putStrLn $ reverse $ take 8 s
        put8 $ Prelude.drop 8 s

ps :: (FiniteBits a) => a -> IO ()
ps = put8 . showWord64

bool :: a -> a -> Bool -> a
bool x _ True  = x
bool _ y False = y
