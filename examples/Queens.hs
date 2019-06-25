module Queens where

import Clash.Prelude
import Data.Maybe

mem <~ (i,x)    = replace i x mem

type Word3 = Unsigned 3

type MbWord3 = Maybe Word3
type Din     = Vec 8 (MbWord3)
type Dout    = Vec 8 Word3

din :: Din
din = $(listToVecTH [ Nothing :: Maybe (Unsigned 3), Just 1, Nothing, Nothing, Just 4, Nothing, Just 6, Just 7 ])

dout0 :: Dout
dout0 = 0 :> 0 :> 0 :> 0 :> 0 :> 0 :> 0 :> 0 :> Nil :: Dout

ind0 :: Word3
ind0 = 0

mbfilter (dout,ind) din = ((dout',ind'), low)
                where

                  f (dout,ind) v = case v of
                                        Just x  -> (dout<~(ind,x), ind+1)
                                        Nothing -> (dout, ind)

                  (dout',ind') = foldl f (dout0,ind0) din

topEntity
  :: Clock System
  -> Reset System
  -> Enable System
  -> Signal System Din
  -> Signal System Bit
topEntity = exposeClockResetEnable (mbfilter `mealy` (dout0, ind0))
