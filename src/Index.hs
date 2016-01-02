module Index where 

import Data.Bits

import Data.Attoparsec.ByteString.Char8
import qualified Text.PrettyPrint.ANSI.Leijen as T
type Index = Int 

type Rank = Int 
type File = Int 

flattenRF :: Rank -> File -> Index
flattenRF r f = r * 8 + f

unflattenRF :: Index -> (Rank,File)
unflattenRF i = (i `shiftR` 3, i .&. 7)

indexToDoc i = T.char (toEnum (f + 97)) T.<> T.char (toEnum (r + 49))
        where (r,f) = unflattenRF i

parseIndex :: Parser Index
parseIndex = parseFileRank combineFR
        where combineFR f r = flattenRF (fromEnum r - 49) (fromEnum f - 97)  

parseFileRank :: (Char -> Char -> a) -> Parser a
parseFileRank f = f <$> satisfy (\c -> 'a' <= c && c <= 'h') <*>
                        satisfy (\c -> '1' <= c && c <= '8')
