module T3290 where

import Clash.Prelude

-- | https://github.com/clash-lang/clash-compiler/issues/3290
--
-- @sequenceA@ (i.e. @traverse#@) over a zero-length vector used to crash
-- normalization with "extractElems must be called with positive number",
-- because 'reduceTraverse' called @extractElems@ with @n == 0@.
topEntity :: Vec 0 (Maybe (Index 3)) -> Maybe (Vec 0 (Index 3))
topEntity = sequenceA
{-# OPAQUE topEntity #-}
