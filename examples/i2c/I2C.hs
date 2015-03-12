module I2C
  (i2c,topEntity)
where

import CLaSH.Prelude

import I2C.MasterBit
import I2C.MasterByte
import I2C.Types

{-# NOINLINE i2c #-}
i2c clkCnt startCmd stopCmd readCmd writeCmd ackIn dIn i2ci =
    (dOut,cmdAck,ackOut,al,busy,i2co)
  where
    (cmd,coreTxd,cmdAck,ackOut,dOut) = masterByteCtrl startCmd stopCmd readCmd
                                        writeCmd ackIn dIn coreAck coreRxd
    (bitOut,i2co)                    = masterBitCtrl clkCnt cmd coreTxd i2ci
    (coreAck,al,busy,coreRxd)        = unbundle bitOut


topEntity = i2c
