{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}

module RecordSumOfProducts where

import Clash.Prelude
import Control.Applicative

data DbState = DbInitDisp (Unsigned 4) | DbWriteRam (Signed 14) (Signed 14)
             | DbDone
    deriving (Show, Eq, Generic, NFDataX)

data DbS = DbS { dbS :: DbState }
  deriving (Generic, NFDataX)


topEntity
  :: Clock System
  -> Reset System
  -> Enable System
  -> Signal System Bit
  -> Signal System Bit
topEntity = exposeClockResetEnable (walkState <^> DbS (DbInitDisp 0))

walkState :: DbS
          -> Bit
          -> (DbS, Bit)

walkState (DbS (DbInitDisp n    )) i = (DbS (DbInitDisp (n+1)  ), 0)
walkState s                        i = (s                       , i)
