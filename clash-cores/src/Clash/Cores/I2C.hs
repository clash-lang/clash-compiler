{-# LANGUAGE CPP #-}

module Clash.Cores.I2C
  ( i2c
  , i2cTop
  , Clash.Cores.I2C.ByteMaster.I2COperation(..)
  ) where

import Clash.Prelude hiding (read)

import Clash.Cores.I2C.BitMaster
import Clash.Cores.I2C.ByteMaster

-- | Core for I2C communication. Returns the output enable signals for SCL en SDA
-- These signals assume that when they are `True`, they pull down SCL and SDA respectively.
-- For 2-wire I2C, you can use BiSignals (`Clash.Signal.Bidirectional.BiSignalIn` and `Clash.Signal.Bidirectional.BiSignalOut`)
-- An example i2c design could look like this:
-- i2cComp clk rst ena sclIn sdaIn = (sclOut, sdaOut)
--  where
--   sclOut = writeToBiSignal sclIn (mux sclOe (pure $ Just 0) (pure Nothing))
--   sdaOut = writeToBiSignal sdaIn (mux sdaOe (pure $ Just 0) (pure Nothing))
--   (sclOe, sdaOe) = unbundle i2cO
--   i2cIn = bundle (readFromBiSignal sclIn, readFromBiSignal sdaIn)
--   (dout,i2cOpAck,busy,al,ackWrite,i2cOut) = i2c clk arst rst ena clkCnt claimBus i2cOp ackRead i2cI
--   ...

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
  -- | Claim bus signal
  "claimBus" ::: Signal dom Bool ->
  -- | I2C operation
  "i2cOp" ::: Signal dom (Maybe I2COperation) ->
  -- | Acknowledge signal to be transmitted from master to slave on read operations
  --   True means SDA is low.
  "ackRead" ::: Signal dom Bool ->
  -- | I2C input signals (SCL, SDA)
  "i2c" ::: Signal dom ("scl" ::: Bit, "sda" ::: Bit) ->
  -- |
  -- 1. Received data
  -- 2. Command acknowledgement
  -- 3. I2C bus busy
  -- 4. Arbitration lost
  -- 5. Received acknowledge signal from slave to master on write operations.
  --    True means SDA is low.
  -- 6. Outgoing I2C signals
  --    6.1 SCL Tri-state signals, Nothing means pulled high.
  --    6.2 SDA Tri-state signals, Nothing means pulled high.
  "" :::
    ( "i2cO" ::: Signal dom (BitVector 8)
    , "i2cOpAck" ::: Signal dom Bool
    , "busy" ::: Signal dom Bool
    , "al" ::: Signal dom Bool
    , "ackWrite" ::: Signal dom Bool
    , "i2cO" ::: Signal dom ("sclOut" ::: Maybe Bit, "sclOut" ::: Maybe Bit))
i2c clk arst rst ena clkCnt claimBus i2cOp ackRead i2cI =
  (dout,i2cOpAck,busy,al,ackWrite,i2cO)

  where
    (i2cOpAck,ackWrite,dout,bitCtrl)
      = byteMaster clk arst enableGen (rst,claimBus,i2cOp,ackRead,bitResp)
    (bitResp,busy,i2cO)
      = bitMaster clk arst enableGen (rst,ena,clkCnt,bitCtrl,i2cI)
    (_cmdAck,al,_dout) = unbundle bitResp
-- See: https://github.com/clash-lang/clash-compiler/pull/2511
{-# CLASH_OPAQUE i2c #-}
