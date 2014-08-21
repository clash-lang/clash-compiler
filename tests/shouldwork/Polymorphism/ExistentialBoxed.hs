{-# LANGUAGE ExistentialQuantification #-}
module ExistentialBoxed where

import Prelude

data Obj = forall a . Obj a (a -> Int)

topEntity = f (Obj 4 (const 8 . id))

f (Obj a g) = g a
