{-# LANGUAGE TemplateHaskell, MultiParamTypeClasses, TypeFamilies #-}
module MoveTypes where 

import Data.Bits
import Data.Word

import Data.Vector.Unboxed 
import Data.Vector.Unboxed.Deriving

import Text.PrettyPrint.ANSI.Leijen

import BitBoard 
import Board 
import Index 

data Move = Move {
        from :: {-# UNPACK #-} !Index, 
        to   :: {-# UNPACK #-} !Index }

moveToDoc :: Move -> Doc 
moveToDoc (Move from to) = indexToDoc from <> indexToDoc to

instance Show Move where 
        show = show . moveToDoc

derivingUnbox "Move"
        [t| Move -> (Int,Int) |] 
        [| \(Move f t) -> (f,t) |]
        [| uncurry Move |]

newtype PromotionType = PromotionType Word8

pattern Knight = PromotionType 0
pattern Bishop = PromotionType 1
pattern Rook   = PromotionType 2
pattern Queen  = PromotionType 3

promotionTypeToDoc :: PromotionType -> Doc 
promotionTypeToDoc Knight = "K"
promotionTypeToDoc Bishop = "B"
promotionTypeToDoc Rook   = "R"
promotionTypeToDoc Queen  = "Q"

data Promotion = Promotion {
        move  :: {-# UNPACK #-} !Move,
        pType :: {-# UNPACK #-} !PromotionType }

promotionToDoc :: Promotion -> Doc 
promotionToDoc (Promotion m pt) = moveToDoc m <> "p=" <> promotionTypeToDoc pt

instance Show Promotion where 
        show = show . promotionToDoc

derivingUnbox "Promotion" 
        [t| Promotion -> (Int,Int,Word8) |]
        [| \(Promotion (Move f t) (PromotionType w)) -> (f,t,w) |]
        [| \(f,t,w) -> Promotion (Move f t) (PromotionType w) |]

data Moves = Moves {
        moves      :: !(Vector Move), 
        promotions :: !(Vector Promotion) } deriving Show 
