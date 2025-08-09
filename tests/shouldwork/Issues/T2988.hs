{-# LANGUAGE DataKinds #-}
{-# LANGUAGE CPP #-}

module T2988 where

import Clash.Prelude
import Prelude ()

topEntity :: Signal System (Vec 4 (Vec 4 (Unsigned 32))) -> Signal System (Vec 4 (Unsigned 32))
topEntity = f
{-# CLASH_OPAQUE topEntity #-}

f :: Signal System (Vec n (Vec 4 (Unsigned 32))) -> Signal System (Vec n (Unsigned 32))
f x = fmap (fmap head) x
{-# CLASH_OPAQUE f #-}
