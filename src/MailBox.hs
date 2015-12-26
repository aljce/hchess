{-# LANGUAGE TemplateHaskell, MultiParamTypeClasses, TypeFamilies #-}
module MailBox where 

import BitBoard 
import Data.Word

import Data.Vector.Unboxed hiding (concat,(++))
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as M
import Data.Vector.Unboxed.Deriving

import Data.Coerce

import Text.PrettyPrint.ANSI.Leijen

newtype Square = S { unS :: Word8 } 

instance Show Square where 
        show = (:[]) . toLetter

toLetter :: Square -> Char 
toLetter (S w) = case w of
        0 -> ' '
        1 -> 'p'
        2 -> 'r'
        3 -> 'n'
        4 -> 'b'
        5 -> 'q'
        6 -> 'k'
        7 -> 'P'
        8 -> 'R'
        9 -> 'N'
        10 -> 'B'
        11 -> 'Q'
        12 -> 'K'

fromLetter :: Char -> Square 
fromLetter = \case 
        ' ' -> noPiece
        'p' -> bPawn
        'r' -> bRook
        'n' -> bKnight
        'b' -> bBishop
        'q' -> bQueen
        'k' -> bKing
        'P' -> wPawn
        'R' -> wRook
        'N' -> wKnight
        'B' -> wBishop
        'Q' -> wQueen
        'K' -> wKing
        _   -> error "Unsupported Char in fromLetter"

allSquareTypes :: [Square]
allSquareTypes@[noPiece,bPawn,bRook,bKnight,bBishop,bQueen,bKing,
                        wPawn,wRook,wKnight,wBishop,wQueen,wKing] = coerce [(0 :: Word8)..12]

derivingUnbox "Square"
        [t| Square -> Word8 |]
        [| unS |]
        [| S   |]

type File    = Int 
type Rank    = Int
type Index   = Int 

indexToDoc :: Index -> Doc
indexToDoc i = let (r,f) = unflattenRF i in char (toEnum (f + 97)) <> char (toEnum (r + 49))
                   
type MailBox = Vector Square

{-# INLINE flattenRF #-}
flattenRF :: Rank -> File-> Index 
flattenRF !r !f = r*8 + f

{-# INLINE unflattenRF #-}
unflattenRF :: Index -> (Rank,File)
unflattenRF !i = divMod i 8

mailBoxToDoc :: MailBox -> [Doc] 
mailBoxToDoc = fmap (string . U.foldr' (:) []) . splitEvery8 0 . U.reverse . U.map toLetter
        where splitEvery8 64 v = []
              splitEvery8 n v = U.take 8 v : splitEvery8 (n + 8) (U.drop 8 v)

{-
mailBoxToBitBoard :: MailBox -> BitBoard 
mailBoxToBitBoard = undefined 

bitBoardToMailBox :: BitBoard -> MailBox 
bitBoardToMailBox (BitBoard bc wc p r n b q k) = undefined 
-}

