module T3311 where

import Clash.Prelude

r :: HiddenClockResetEnable dom => Signal dom Bool
r = register True $ not <$> r
{-# OPAQUE r #-}

topEntity :: HiddenClockResetEnable System => Signal System Bool
topEntity = r
{-# OPAQUE topEntity #-}
