module I2C where

import CLaSH.Prelude

import I2C.BitMaster
import I2C.ByteMaster
import I2C.Types

i2c rst ena clkCnt start stop read write ackIn din i2cI = (dout,hostAck,busy,al,ackOut,i2cO)
  where
    (hostAck,ackOut,dout,bitCtrl) = mealyB byteMasterT byteMasterInit (rst,start,stop,read,write,ackIn,din,bitResp)
    (bitResp,busy,i2cO)           = mealyB bitMasterT  bitMasterInit  (rst,ena,clkCnt,bitCtrl,i2cI)
    (cmdAck,al,dbout)             = unbundle bitResp

topEntity
  :: SystemClockReset
  => Signal System Bool
  -> Signal System Bool
  -> Signal System (Unsigned 16)
  -> Signal System Bool
  -> Signal System Bool
  -> Signal System Bool
  -> Signal System Bool
  -> Signal System Bool
  -> Signal System (BitVector 8)
  -> Signal System (Bit, Bit)
  -> (Signal System (BitVector 8), Signal System Bool,
      Signal System Bool, Signal System Bool, Signal System Bool,
      Signal System (Bit, Bool, Bit, Bool))
topEntity = i2c
