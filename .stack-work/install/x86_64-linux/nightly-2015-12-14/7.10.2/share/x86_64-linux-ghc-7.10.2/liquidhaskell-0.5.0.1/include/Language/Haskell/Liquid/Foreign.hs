{-# LANGUAGE MagicHash #-}

{- OPTIONS_GHC -cpp #-}
{- OPTIONS_GHC -cpp -fglasgow-exts -}

module Language.Haskell.Liquid.Foreign where

import Foreign.C.Types          (CSize(..))
import Foreign.Ptr
import Foreign.ForeignPtr
import GHC.Base

-- TODO: shouldn't have to re-import these (tests/pos/imp0.hs)
{- import Foreign.C.Types    -}    
{- import Foreign.Ptr        -}
{- import Foreign.ForeignPtr -}
{- import GHC.Base           -}



-----------------------------------------------------------------------------------------------

{-# NOINLINE intCSize #-}
{-@ assume intCSize :: x:Int -> {v: CSize | v = x } @-}
intCSize :: Int -> CSize
intCSize = fromIntegral 

{-# NOINLINE cSizeInt #-}
{-@ assume cSizeInt :: x:CSize -> {v: Int | v = x } @-}
cSizeInt :: CSize -> Int
cSizeInt = fromIntegral 


{-@ assume mkPtr :: x:GHC.Prim.Addr# -> {v: (Ptr b) | ((plen v) = (addrLen x) && ((plen v) >= 0)) } @-}
mkPtr   :: Addr# -> Ptr b
mkPtr = undefined -- Ptr x 


{-@ isNullPtr :: p:(Ptr a) -> {v:Bool | ((Prop v) <=> (isNullPtr p)) } @-}
isNullPtr :: Ptr a -> Bool
isNullPtr p = (p == nullPtr)
{-# INLINE isNullPtr #-}

{-@ fpLen :: p:(ForeignPtr a) -> {v:Int | v = (fplen p) } @-}
fpLen :: ForeignPtr a -> Int
fpLen = undefined

{-@ pLen :: p:(Ptr a) -> {v:Int | v = (plen p) } @-}
pLen :: Ptr a -> Int
pLen = undefined

{-@ deref :: p:Ptr a -> {v:a | v = (deref p)} @-}
deref :: Ptr a -> a
deref = undefined

{-@ eqPtr :: p:PtrV a
          -> q:{v:PtrV a | (((pbase v) = (pbase p)) && ((plen v) <= (plen p)))}
          -> {v:Bool | ((Prop v) <=> ((plen p) = (plen q)))}
  @-}
eqPtr :: Ptr a -> Ptr a -> Bool
eqPtr = undefined
