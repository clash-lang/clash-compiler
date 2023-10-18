{-# LANGUAGE CPP #-}

module Clash.Cores.I2C where

import Clash.Prelude hiding (read)

import Clash.Cores.I2C.BitMaster
import Clash.Cores.I2C.ByteMaster

-- | Core for I2C communication
i2c ::
  forall dom .
  KnownDomain dom =>
  -- | Input Clock
  "clk" ::: Clock dom ->
  -- | Low level reset
  "arst" ::: Reset dom ->
  -- | Statemachine reset
  "rst" ::: Signal dom Bool ->
  -- | BitMaster enable
  "ena" ::: Signal dom Bool ->
  -- | Clock divider
  "clkCnt" ::: Signal dom (Unsigned 16) ->
  -- | Start signal
  "start" ::: Signal dom Bool ->
  -- | Stop signal
  "stop" ::: Signal dom Bool ->
  -- | Read signal
  "read" ::: Signal dom Bool ->
  -- | Write signal
  "write" ::: Signal dom Bool ->
  -- | Ack signal
  "ackIn" ::: Signal dom Bool ->
  -- | Input data
  "din" ::: Signal dom (BitVector 8) ->
  -- | I2C input signals (SCL, SDA)
  "i2c" ::: Signal dom ("scl" ::: Bit, "sda" ::: Bit) ->
  -- |
  -- 1. Received data
  -- 2. Command acknowledgement
  -- 3. I2C bus busy
  -- 4. Arbitration lost
  -- 5. I2C slave acknowledgement
  -- 6. Outgoing I2C signals
  --    6.1 SCL
  --    6.2 SCL Output enable`
  --    6.3 SDA
  --    6.4 SDA Output enable
  "" :::
    ( "i2cO" ::: Signal dom (BitVector 8)
    , "scl" ::: Signal dom Bool
    , "sclOEn" ::: Signal dom Bool
    , "sda" ::: Signal dom Bool
    , "sdaOEn" ::: Signal dom Bool
    , "i2cO" ::: Signal dom ("scl" ::: Bit, "sclOEn" ::: Bool, "sda" ::: Bit, "sdaOEn" ::: Bool))
i2c clk arst rst ena clkCnt start stop read write ackIn din i2cI = (dout,hostAck,busy,al,ackOut,i2cO)
  where
    (hostAck,ackOut,dout,bitCtrl) = byteMaster clk arst enableGen (rst,start,stop,read,write,ackIn,din,bitResp)
    (bitResp,busy,i2cO)           = bitMaster  clk arst enableGen (rst,ena,clkCnt,bitCtrl,i2cI)
    (_cmdAck,al,_dbout)           = unbundle bitResp
-- See: https://github.com/clash-lang/clash-compiler/pull/2511
{-# CLASH_OPAQUE i2c #-}
