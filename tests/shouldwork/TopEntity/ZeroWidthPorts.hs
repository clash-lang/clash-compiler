-- | A top entity whose ports are all zero-width, and so has no ports in HDL.
-- The VHDL backend used to omit the @end;@ of such an entity.
module ZeroWidthPorts where

import Clash.Prelude

zeroWidth :: Unsigned 0 -> Vec 0 Bit
zeroWidth _ = Nil
{-# OPAQUE zeroWidth #-}

topEntity :: Vec 0 (Unsigned 8) -> Unsigned 0 -> (Vec 0 Bit, ())
topEntity _ u = (zeroWidth u, ())
