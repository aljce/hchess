{-# LANGUAGE LambdaCase #-}
module Board (Board(..), emptyBoard, parseBoard) where 

import Prelude hiding (takeWhile)

import Data.Word
import Data.Bits
import Data.Attoparsec.ByteString.Char8
import Control.Applicative

data Board = Board {-# UNPACK #-} !Word64 --Black Piece Set 
                   {-# UNPACK #-} !Word64 --White Piece Set
                   {-# UNPACK #-} !Word64 --Pawns
                   {-# UNPACK #-} !Word64 --Rooks 
                   {-# UNPACK #-} !Word64 --Knights 
                   {-# UNPACK #-} !Word64 --Bishops
                   {-# UNPACK #-} !Word64 --Queens 
                   {-# UNPACK #-} !Word64 --Kings
                   deriving (Show)

emptyBoard :: Board 
emptyBoard = Board 0 0 0 0 0 0 0 0 

showWord64 :: Word64 -> String 
showWord64 w = reverse $ map (\i -> if testBit w i then '1' else '0') [0..63]

put8 :: String -> IO ()
put8 [] = return ()
put8 s = do
        putStrLn $ Prelude.take 8 s
        put8 $ Prelude.drop 8 s

charUpdate :: Int -> Board -> Char -> Board 
charUpdate i (Board bc wc p r n b q k) = \case
        'p' -> Board (setBit bc i) wc (setBit p i) r n b q k 
        'r' -> Board (setBit bc i) wc p (setBit r i) n b q k
        'n' -> Board (setBit bc i) wc p r (setBit n i) b q k
        'b' -> Board (setBit bc i) wc p r n (setBit b i) q k
        'q' -> Board (setBit bc i) wc p r n b (setBit q i) k
        'k' -> Board (setBit bc i) wc p r n b q (setBit k i)
        'P' -> Board bc (setBit wc i) (setBit p i) r n b q k
        'R' -> Board bc (setBit wc i) p (setBit r i) n b q k
        'N' -> Board bc (setBit wc i) p r (setBit n i) b q k
        'B' -> Board bc (setBit wc i) p r n (setBit b i) q k
        'Q' -> Board bc (setBit wc i) p r n b (setBit q i) k
        'K' -> Board bc (setBit wc i) p r n b q (setBit k i) 
        ' ' -> Board bc wc p r n b q k


parseBoard :: Parser Board 
parseBoard = snd . toBoard . concat <$> many' (pieces <|> blank <|> slash) 
        where toBoard :: String -> (Int,Board) 
              toBoard = foldr (\c (i,b) -> (i+1,charUpdate i b c)) (0,emptyBoard)
              pieces :: Parser String
              pieces = (:[]) <$> satisfy (\c -> ('a' <= c && c <= 'z') || ('A' <= c && c <= 'Z')) 
              blank :: Parser String
              blank = (flip replicate ' ' . subtract 48 . fromEnum) <$> satisfy isDigit
              slash = [] <$ char '/'

