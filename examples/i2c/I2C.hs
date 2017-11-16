module I2C where

import Clash.Prelude

import I2C.BitMaster
import I2C.ByteMaster
import I2C.Types

{-# ANN i2c
  (defTop
    { t_name     = "i2c"
    , t_inputs   = [ PortName "arst"
                   , PortName "clk"
                   , PortName "rst"
                   , PortName "ena"
                   , PortName "clkCnt"
                   , PortName "start"
                   , PortName "stop"
                   , PortName "read"
                   , PortName "write"
                   , PortName "ackIn"
                   , PortName "din"
                   , PortName "i2cI"]
    , t_output   = PortField ""
                     [ PortName "dout"
                     , PortName "hostAck"
                     , PortName "busy"
                     , PortName "al"
                     , PortName "ackOut"
                     , PortField "" [PortName "i2cO_clk"]
                     ]
    }) #-}
i2c clk arst rst ena clkCnt start stop read write ackIn din i2cI = (dout,hostAck,busy,al,ackOut,i2cO)
  where
    (hostAck,ackOut,dout,bitCtrl) = byteMaster clk arst (rst,start,stop,read,write,ackIn,din,bitResp)
    (bitResp,busy,i2cO)           = bitMaster  clk arst (rst,ena,clkCnt,bitCtrl,i2cI)
    (cmdAck,al,dbout)             = unbundle bitResp
