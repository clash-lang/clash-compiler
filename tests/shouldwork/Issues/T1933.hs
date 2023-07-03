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
-- See: https://github.com/clash-lang/clash-compiler/pull/2511
{-# CLASH_OPAQUE f #-}

p :: Unsigned 12
p = case clockGen @System of
      Clock _ -> 4
-- See: https://github.com/clash-lang/clash-compiler/pull/2511
{-# CLASH_OPAQUE p #-}

q :: Natural
#if __GLASGOW_HASKELL__ == 900
q = case p of U n -> n
#else
q = coerce p
#endif
-- See: https://github.com/clash-lang/clash-compiler/pull/2511
{-# CLASH_OPAQUE q #-}

topEntity :: Unsigned 12 -> T
topEntity x =
#if __GLASGOW_HASKELL__ == 900
  let r = fromIntegral q :: Unsigned 12
#else
  let r = coerce q :: Unsigned 12
#endif
      {-# NOINLINE r #-}
   in f (MkT x r)
