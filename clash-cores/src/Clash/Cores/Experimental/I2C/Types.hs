{-|
  Copyright   :  (C) 2014, University of Twente
                     2024, Google LLC
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>
-}

module Clash.Cores.Experimental.I2C.Types where

import Clash.Prelude

-- | I2C commands: start, stop, read, write, and no-op.
data I2CCommand = I2Cstart | I2Cstop | I2Cwrite | I2Cread | I2Cnop
  deriving (Eq, Ord, Generic, NFDataX)

-- | Bit-level I2C control signals (Command, Bit).
type BitCtrlSig = (I2CCommand, Bit)

-- | Bit-level I2C response signals (Ack, Busy, Bit).
type BitRespSig = (Bool, Bool, Bit)

-- | I2C input signals (SCL, SDA).
type I2CIn = (Bit, Bit)

-- | I2C output Tri-state signals (SCL, SDA)
-- Since I2C is a protocol with pull ups, Nothing means pulled high.
type I2COut = (Maybe Bit, Maybe Bit)
