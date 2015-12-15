{-# LANGUAGE BangPatterns #-}
module GameTree where 

import Prelude hiding (Foldable())
import Data.Functor.Foldable 

import Data.IntMap.Strict hiding (foldr)
import qualified Data.IntMap.Strict as I
import Data.Sequence hiding (empty,singleton,length)
import qualified Data.Sequence as S
import Data.Monoid

import Data.Coerce

import Board 
import Tree 

type Moves = IntMap (Endo (Seq Int))

data Move = M !Board {-# UNPACK #-} !Int

test n = Tree.take n $ flip index 0 $ snd $ findMin $ generateTree startingBoard

main :: IO ()
main = print $ length $ test 7
{-
generateTree2 :: Board -> Forest Int 
generateTree2 b = mapWithKey 
        where coalg :: Board -> Base (Tree Int) (IntMap (Seq (Board))
              coalg = BTree 
-}

generateTree :: Board -> Forest Int 
generateTree board = mapWithKey (fmap . unfoldMoves) $ generateMoves board 
        where {-# INLINE unfoldMoves #-}
              unfoldMoves :: Int -> Int -> Tree Int 
              unfoldMoves start end = ana coalg (M (applyMove start end board) end)
              {-# INLINABLE coalg #-}
              coalg :: Move -> Base (Tree Int) Move
              coalg (M !b !end) = BTree end (mapWithKey (fmap . buildMove b) (generateMoves b)) 
              {-# INLINE buildMove #-}
              buildMove !board !start !end = M (applyMove start end board) end

generateMoves :: Board -> IntMap (Seq Int)
generateMoves b = flip appEndo S.empty <$> allMoves
        where allMoves = foldrWithKey (\k p accum -> unionWith (<>) (moves b k p) accum) empty b 

{-# INLINE applyMove #-}
applyMove :: Int -> Int -> Board -> Board 
applyMove !start !end !board = insert end (board ! start) (delete start board) 

type MakeMoves = Board -> Int -> Color -> Moves

moves :: Board -> Int -> Piece -> Moves
moves !board !index (Piece !c (PieceType !pt))
        | pt == 1 = movePawn board index c
        | pt == 2 = moveRook board index c
        | pt == 3 = moveKnight board index c 
        | pt == 4 = moveBishop board index c
        | pt == 5 = moveQueen board index c
        | pt == 6 = moveKing board index c

singletonDList :: a -> Endo (Seq a)
singletonDList = Endo . (<|) 

movePawn :: MakeMoves 
movePawn board index True 
        | index <= 56 = singleton index (singletonDList (index + 8))  
        | otherwise = empty 
movePawn board index False = empty

moveRook :: MakeMoves 
moveRook board index c = empty  

moveKnight :: MakeMoves 
moveKnight board index c = empty 

moveBishop :: MakeMoves
moveBishop board index c = empty 

moveQueen :: MakeMoves
moveQueen board index c = empty 

moveKing :: MakeMoves
moveKing board index c = empty 
