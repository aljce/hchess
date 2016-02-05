module StrictPair where

data a :*: b = Pair !a !b

uncurrySP :: (a -> b -> c) -> (a :*: b -> c)
uncurrySP f (Pair a b) = f a b 

currySP :: (a :*: b -> c) -> a -> b -> c
currySP f a b = f (Pair a b)
