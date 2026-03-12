module T3159 where

import Clash.Prelude

foo :: HiddenClockResetEnable System => Signal System (Maybe Bit)
foo =
  Just
    <$> blockRam
      (singleton undefined)
      (pure 0)
      (pure (Nothing :: Maybe (Index 1, Bit)))

topEntity = exposeClockResetEnable (maybe 0 id <$> foo)
