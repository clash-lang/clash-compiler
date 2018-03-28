{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}

module RecordSumOfProducts where

import Clash.Prelude
import Control.Applicative

data DbState = DbInitDisp (Unsigned 4) | DbWriteRam (Signed 14) (Signed 14)
             | DbDone
    deriving (Show, Eq)

data DbS = DbS { dbS :: DbState }


topEntity
  :: Clock System Source
  -> Reset System Asynchronous
  -> Signal System Bit -> Signal System Bit
topEntity = exposeClockReset (walkState <^> DbS (DbInitDisp 0))

walkState :: DbS
          -> Bit
          -> (DbS, Bit)

walkState (DbS (DbInitDisp n    )) i = (DbS (DbInitDisp (n+1)  ), 0)
walkState s                        i = (s                       , i)
