{-# LANGUAGE DataKinds #-}
module RegisterFail where

import Clash.Prelude

topEntity
  :: Clock  System
  -> Reset  System
  -> Enable System
  -> Signal System (Signed 4)
  -> Signal System (Signed 4)
  
topEntity = exposeClockResetEnable regFail
  where    
    reg i = register 0 i
    
    count ::
      HiddenClockResetEnable dom =>
      Signal dom (Signed 3)
    count =
      register 0 ((+1) <$> count)
    
    regFail ::
      HiddenClockResetEnable dom =>
      Signal dom (Signed 4) ->
      Signal dom (Signed 4)
      
    regFail =
      mux ((== 4) <$> count) 0 . reg    
   
    
