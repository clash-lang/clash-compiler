{-# LANGUAGE DataKinds #-}
module Register where

import Clash.Prelude

topEntity
  :: Clock  System
  -> Reset  System
  -> Enable System
  -> Signal System (Signed 4)
  -> Signal System (Signed 4)
  
topEntity = exposeClockResetEnable reg
  where
    reg i = register 0 i
