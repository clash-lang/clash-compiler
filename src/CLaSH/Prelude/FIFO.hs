module CLaSH.Prelude.FIFO where

import Control.Applicative
import Control.Arrow
import Control.Category
import Prelude hiding ((.),id)

import CLaSH.Signal.Implicit

newtype FIFO i o = FIFO { runFIFO :: Signal Bool -> Signal Bool -> Signal i -> (Signal Bool, Signal Bool, Signal o) }

instance Category FIFO where
  id = FIFO (\valid ready dataIn -> (valid,ready,dataIn))
  (FIFO f2) . (FIFO f1) = FIFO f3
    where
      f3 f1ValIn f2ReadyIn f1DataIn = (f2ValOut,f1ReadyOut,f2DataOut)
        where
          (f1ValOut,f1ReadyOut,f1DataOut) = f1 f1ValIn  f2ReadyOut f1DataIn
          (f2ValOut,f2ReadyOut,f2DataOut) = f2 f1ValOut f2ReadyIn  f1DataOut

instance Arrow FIFO where
  arr f = FIFO (\valid ready dataIn -> (valid,ready,f <$> dataIn))

  first (FIFO f) = FIFO f'
    where
      f' valIn readyIn dataIn = (valOut,readyOut,pack (dOut,dInR))
        where
          (dInL,dInR)            = unpack dataIn
          (valOut,readyOut,dOut) = f valIn readyIn dInL

  second (FIFO f) = FIFO f'
    where
      f' valIn readyIn dataIn = (valOut,readyOut,pack (dInL,dOut))
        where
          (dInL,dInR)            = unpack dataIn
          (valOut,readyOut,dOut) = f valIn readyIn dInR

  (FIFO f1) *** (FIFO f2) = FIFO f3
    where
      f3 valIn readyIn dataIn = ( (&&) <$> f1ValOut <*> f2ValOut
                                , (&&) <$> f1ReadyOut <*> f2ReadyOut
                                , pack (f1DataOut,f2DataOut)
                                )
        where
          (dInL,dInR)                     = unpack dataIn
          (f1ValOut,f1ReadyOut,f1DataOut) = f1 valIn readyIn dInL
          (f2ValOut,f2ReadyOut,f2DataOut) = f2 valIn readyIn dInR

  (FIFO f1) &&& (FIFO f2) = FIFO f3
    where
      f3 valIn readyIn dataIn = ( (&&) <$> f1ValOut <*> f2ValOut
                                , (&&) <$> f1ReadyOut <*> f2ReadyOut
                                , pack (f1DataOut,f2DataOut)
                                )
        where
          (f1ValOut,f1ReadyOut,f1DataOut) = f1 valIn readyIn dataIn
          (f2ValOut,f2ReadyOut,f2DataOut) = f2 valIn readyIn dataIn
