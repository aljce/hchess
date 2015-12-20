{-# LANGUAGE TypeOperators #-}
module Naturals where

import GHC.TypeLits

newtype T (n :: Nat) = T Int 

test2 :: (2 <= 3) => T (4 + 3)
test2 = natVal (Proxy :: 4 + 3)

test :: T (2 + 3)
test = T 0 
