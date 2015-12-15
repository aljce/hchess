{-# LANGUAGE TypeFamilies, LambdaCase, TupleSections #-}
module Tree where 

import Prelude hiding (Foldable())
import qualified Data.Foldable as F
import Data.Functor.Foldable

import Data.IntMap.Strict hiding ()
import qualified Data.IntMap.Strict as I
import Data.Sequence hiding (zipWith)
import qualified Data.Sequence as S
import Data.Monoid

type Forest a = IntMap (Seq (Tree a))

data Tree a = Tree a (Forest a)

instance (Show a) => Show (Tree a) where 
        show = unlines . draw . fmap show 
                where draw (Tree leaf branches) = leaf : drawSubTrees (collapseForest branches)
                      collapseForest = I.foldr (\v accum -> F.toList v ++ accum) [] 
                      drawSubTrees []     = []
                      drawSubTrees [t]    = "|" : shift "`- " "   " (draw t)
                      drawSubTrees (t:ts) = "|" : shift "+- " "|  " (draw t) ++ 
                                                    drawSubTrees ts
                      shift first other = zipWith (++) (first : repeat other)

instance F.Foldable Tree where 
        foldMap f (Tree leaf branches) = f leaf <> foldMap (foldMap (foldMap f)) branches

instance Functor Tree where 
        fmap f (Tree leaf branches) = Tree (f leaf) (I.map (fmap (fmap f)) branches)

instance Traversable Tree where 
        traverse f (Tree leaf branches) 
                = Tree <$> f leaf <*> traverse (traverse (traverse f)) branches

data BTree a b = BTree a (IntMap (Seq b)) 

instance Functor (BTree a) where 
        fmap f (BTree leaf branches) = BTree leaf (I.map (fmap f) branches)

type instance Base (Tree a) = BTree a

instance Foldable (Tree a) where 
        project (Tree leaf branches) = BTree leaf branches

instance Unfoldable (Tree a) where 
        embed (BTree leaf branches) = Tree leaf branches

take :: Int -> Tree a -> Tree a
take n = ana coalg . (n,)
        where coalg (n,Tree leaf branches) 
                | n <= 0    = BTree leaf I.empty 
                | otherwise = BTree leaf (I.map (fmap (n-1,)) branches)

testTree :: Int -> Tree Int
testTree = ana $ \i -> 
        BTree i (if i > 9 then I.empty else I.singleton i (S.fromList [i+1,i+2])) 
