{-# LANGUAGE CPP #-}
module T1933 where

import Clash.Prelude
import Clash.Sized.Internal.Unsigned
import Data.Coerce
import GHC.Natural
import Clash.Signal.Internal

data T = MkT (Unsigned 12) (Unsigned 12)

f :: T -> T
f x = x
{-# OPAQUE f #-}

p :: Unsigned 12
p = case clockGen @System of
      Clock _ -> 4
{-# OPAQUE p #-}

q :: Natural
q = coerce p
{-# OPAQUE q #-}

topEntity :: Unsigned 12 -> T
topEntity x =
  let r = coerce q :: Unsigned 12
      {-# NOINLINE r #-}
   in f (MkT x r)
